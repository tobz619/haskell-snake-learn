{-# LANGUAGE BangPatterns #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module UI.HighscoreScreens where

import Brick
import qualified Brick.AttrMap as A
import Brick.Forms
import Brick.Main as M
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Dialog (Dialog)
import qualified Brick.Widgets.Dialog as D
import qualified Brick.Widgets.List as L
import Control.Monad (join, when, forever)
import Control.Monad.IO.Class (MonadIO (liftIO))
import DB.Client (leaderBoardRequest, recvReplayData)
import DB.Highscores
  ( dbPath,
    getScoreSlice,
    getScores,
    maxDbSize,
  )
import DB.Types (PageHeight (..), PageNumber (..), ScoreField (..))
import Data.Coerce (coerce)
import Data.Functor ((<&>))
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time
import qualified Data.Vector as V
import Data.Word (Word8)
import Database.SQLite.Simple (withConnection)
import qualified Database.SQLite.Simple as DB
import qualified Graphics.Vty as V
import Lens.Micro ((%~), (.~), (^.))
import Lens.Micro.Mtl (preview, use, (%=), (.=))
import Lens.Micro.TH (makeLenses)
import UI.ReplayPlayer
import Data.List (nub)
import Control.Concurrent (threadDelay, forkIO)
import Brick.BChan
import UI.Types (Tick(..))

data HSPageName = ScoreTable | HSDialogNum Int | ReplayIndex | InvalidIndex
  deriving (Show, Eq, Ord)

data Mode = Page | ShowingAmountStateDialog | ShowingViewReplayDialog | ReloadingScores | FetchingScores

newtype ShowAmountStateDialog = ShowAmountStateDialog
  { _menuDialog :: Dialog Int HSPageName
  }

newtype ViewReplayForm = ViewReplayForm
  { _replayIndex :: Word8
  }
  deriving newtype (Show, Read, Num)

data HighScoreState = HighScoreState
  { _pageNumber :: !PageNumber,
    _pageHeight :: !PageHeight,
    _scoreArr :: V.Vector (Int, ScoreField),
    _scorePageList :: L.List HSPageName (Int, ScoreField),
    _selectAmount :: ShowAmountStateDialog,
    _selectReplay :: Form ViewReplayForm Tick HSPageName,
    _mode :: !Mode
  }

concat
  <$> mapM
    makeLenses
    [ ''HighScoreState,
      ''ShowAmountStateDialog,
      ''ViewReplayForm
    ]

defHeight :: Int
defHeight = 5

ui :: HighScoreState -> [Widget HSPageName]
ui hss = case hss ^. mode of
  Page -> [stats, scores]
  FetchingScores -> [stats, undefined, scores]
  ReloadingScores -> [stats, reload, scores]
  ShowingViewReplayDialog -> [formWidget, scores]
    where
      formWidget = C.centerLayer $ renderForm (hss ^. selectReplay)
  ShowingAmountStateDialog ->
    [diaWidget (hss ^. selectAmount), scores]
  where
    scores = allList (hss ^. pageHeight) (hss ^. pageNumber) (hss ^. scorePageList)
    diaWidget (ShowAmountStateDialog dia) = D.renderDialog dia emptyWidget
    reload = C.centerLayer $ B.border $ str "Reloading scores"
    stats =
      vBox $
        [ str $ "PageNumber " ++ show (hss ^. pageNumber),
          str $ "PageHeight " ++ show (hss ^. pageHeight),
          str $ "ScoreArrIxs" ++ show (fst <$> hss ^. scoreArr)
        ]

mkScoresList :: PageNumber -> PageHeight -> V.Vector (Int, ScoreField) -> L.List HSPageName (Int, ScoreField)
mkScoresList (PageNumber pn) (PageHeight ph) scores = L.list ScoreTable (V.take ph . V.dropWhile ((/= ph*pn) . fst) $ scores) 2

allList :: (Traversable t, L.Splittable t, Ord n, Show n) => PageHeight -> PageNumber -> L.GenericList n t (Int, ScoreField) -> Widget n
allList pHeight pNum scoreList =
  C.center . vBox . (hLimit 75 . B.border <$>) $
    [ C.hCenter . withAttr headerAttr $ txt "HIGH SCORES",
      renderScoresList pHeight pNum scoreList
    ]

renderScoresList :: (Traversable t, L.Splittable t, Ord n, Show n) => PageHeight -> PageNumber -> L.GenericList n t (Int, ScoreField) -> Widget n
renderScoresList pHeight pNum l =  vBox $ [
    vLimit (2 * coerce pHeight) . L.renderList f True $ l, 
    str ("Page: " ++ show (pNum + 1))
    ]
  where
    f sel s
      | sel = withAttr D.buttonAttr $ lister s
      | otherwise = lister s
    lister (ix, ScoreField _ n s d _ rep) =
      ( \btn ->
          hBox $
            padding
              <$> [ hLimit 3 $ strWrap (show $ ix + 1),
                    hLimit 3 $ txtWrap n,
                    hLimit 3 $ handleScore s,
                    hLimit 14 $ handleDate d,
                    padLeft (Pad 2) $ hLimit 5 btn
                  ]
      )
        (maybe (txt "N/A") (const replayButton) rep)

    padding = padBottom (Pad 1) . padRight Max

    handleScore = txtWrap . Text.pack . show

    handleDate = txtWrap . formatDbIntToTime

    replayButton = withAttr headerAttr $ txtWrap "VIEW"

formatDbIntToTime :: Int -> Text
formatDbIntToTime posixTime =
  let !utcTime = secondsToNominalDiffTime (fromIntegral posixTime) `addUTCTime` UTCTime (ModifiedJulianDay 0) 0 -- Convert to UTC Time
   in Text.pack $ formatTime (defaultTimeLocale {knownTimeZones = [read "GMT"]}) "%T %u %b" utcTime

inputHandler :: BrickEvent HSPageName Tick -> EventM HSPageName (HighScoreState) ()
inputHandler (AppEvent Tick) = pure ()
inputHandler ev = do
  !m <- use mode
  case m of
    ShowingViewReplayDialog -> handleViewReplayForm ev
    ShowingAmountStateDialog -> handleEventDialog ev
    Page -> handleEventMain ev
    ReloadingScores -> do
      pn <- use pageNumber
      ph <- use pageHeight
      s <- (liftIO . fmap V.fromList . withConnection dbPath) $ \conn -> (getPages pn ph conn)
      scoreArr .= s
      join $ changeScoreArr <$> use pageNumber <*> use pageHeight <*> use scoreArr
      mode .= Page
    FetchingScores -> handleEventMain ev
  pure ()

handleEventMain :: BrickEvent HSPageName Tick -> EventM HSPageName (HighScoreState) ()
handleEventMain (VtyEvent (V.EvKey (V.KChar 'q') [])) = M.halt
handleEventMain (VtyEvent (V.EvKey V.KEsc [])) = M.halt
handleEventMain (VtyEvent (V.EvKey (V.KChar 'r') [])) = do 
  mode .= ReloadingScores


handleEventMain (VtyEvent (V.EvKey (V.KChar '/') [])) = do
  mode .= ShowingViewReplayDialog
handleEventMain (VtyEvent (V.EvKey (V.KChar 'h') [])) = do
  mode .= ShowingAmountStateDialog
  selectAmount .= defShowAmountStateDialog

handleEventMain (VtyEvent (V.EvKey V.KLeft [])) = do
  pageNumber %= (\(PageNumber !n) -> PageNumber (max 0 (n - 1)))
  join $ changeScoreArr <$> use pageNumber <*> use pageHeight <*> use scoreArr

handleEventMain (VtyEvent (V.EvKey V.KRight [])) = do
  (PageHeight hei) <- use pageHeight
  pageNumber %= (\(PageNumber !n) -> PageNumber (min ((fromIntegral maxDbSize) `div` hei) (n + 1)))
  join $ changeScoreArr <$> use pageNumber <*> use pageHeight <*> use scoreArr
  
handleEventMain (VtyEvent ev) = zoom scorePageList $ L.handleListEvent ev
handleEventMain _ = return ()

handleEventDialog :: BrickEvent HSPageName Tick -> EventM HSPageName (HighScoreState) ()
handleEventDialog (VtyEvent (V.EvKey V.KEnter [])) = do
  d <- fromMaybe defDialog . preview (selectAmount . menuDialog) <$> get
  pageHeight .= (PageHeight . maybe defHeight snd . D.dialogSelection $ d)
  pageNumber .= 0
  ph <- use pageHeight
  s <- liftIO $ V.fromList <$> withConnection dbPath (getPages (PageNumber 0) ph)
  scoreArr .= s
  list <- mkScoresList <$> use pageNumber <*> use pageHeight <*> use scoreArr
  scorePageList .= list 
  mode .= Page
handleEventDialog (VtyEvent (V.EvKey V.KEsc [])) = do
  mode .= Page
handleEventDialog (VtyEvent ev) = zoom (selectAmount . menuDialog) $ D.handleDialogEvent ev
handleEventDialog _ = do
  m <- use mode
  case m of
    _ -> return ()

handleViewReplayForm :: BrickEvent HSPageName Tick -> EventM HSPageName (HighScoreState) ()
handleViewReplayForm (VtyEvent (V.EvKey (V.KChar 'q') [])) = mode .= Page
handleViewReplayForm (VtyEvent (V.EvKey V.KEnter [])) = do
  f <- use selectReplay
  scores <- use scoreArr
  let (ViewReplayForm index) = formState f
      mbScoreField = snd <$> scores V.!? (fromIntegral index - 1)
  if not . isJust $ (getReplay =<< mbScoreField)
    then do
      selectReplay .= setFieldValid False ReplayIndex f
      mode .= ShowingViewReplayDialog
    else do
      let scoreID = getScoreFieldID $ fromJust mbScoreField
      selectReplay .= setFieldValid True ReplayIndex f
      mbReplay <- liftIO (recvReplayData scoreID)
      suspendAndResume' $ mapM_ (liftIO . replayFromReplayData) mbReplay
      mode .= Page
handleViewReplayForm ev = zoom selectReplay $ handleFormEvent ev

theMap :: AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (headerAttr, fg V.white),
      (cellAttr, V.red `on` V.white),
      (bgAttr, bg V.red),
      (D.dialogAttr, fg V.white),
      (D.buttonAttr, V.red `on` V.white),
      (D.buttonSelectedAttr, V.white `on` V.red),
      (focusedFormInputAttr, V.yellow `on` V.black),
      (invalidFormInputAttr, V.black `on` V.yellow)
    ]

headerAttr, cellAttr, bgAttr :: AttrName
headerAttr = bgAttr <> attrName "header"
cellAttr = attrName "cell"
bgAttr = attrName "bg"

highScoresApp :: App HighScoreState Tick HSPageName
highScoresApp =
  M.App
    { M.appDraw = ui,
      M.appChooseCursor = M.neverShowCursor,
      M.appHandleEvent = inputHandler,
      M.appStartEvent = pure (),
      M.appAttrMap = const theMap
    }

selectReplayForm :: ViewReplayForm -> Form ViewReplayForm e HSPageName
selectReplayForm =
  newForm fields
  where
    fields = [heading @@= editShowableField replayIndex ReplayIndex]
    validations :: Word8 -> Bool
    validations x = all ($ x) [(>= 1), (<= maxDbSize + 1) . fromIntegral]
    heading = (<=>) (padBottom (Pad 1) $ withAttr headerAttr $ txt "Select the number of the replay to watch: ")

defDialog :: Dialog Int HSPageName
defDialog =
  D.dialog
    (Just $ txt "How many scores to show per page?")
    (Just (HSDialogNum defHeight, options))
    60
  where
    options = (\n -> (show n, HSDialogNum n, n)) <$> [5, 10, 25]

defShowAmountStateDialog :: ShowAmountStateDialog
defShowAmountStateDialog = ShowAmountStateDialog defDialog

getPages :: PageNumber -> PageHeight -> DB.Connection -> IO [(Int, ScoreField)]
getPages (PageNumber n) (PageHeight h) dbConn = do
  let indices = concatMap scoreIx pns
  zip indices <$> getScoreSlice (length indices) (PageNumber n) (PageHeight h) dbConn
    where scoreIx pn = [pn * h .. pn * h + h - 1]
          pns = [max 0 (n - 2) .. n + 2]

changeScoreArr :: PageNumber -> PageHeight -> V.Vector (Int, ScoreField) -> EventM n (HighScoreState) ()
changeScoreArr pn@(PageNumber pNum) ph@(PageHeight pHei) scorePages
  | Nothing <- V.find  (== (pNum * pHei + 1)) (fst <$> scorePages) = do
      viewList <- liftIO (V.fromList <$> withConnection dbPath (getPages pn ph))
      scoreArr .= viewList
      scorePageList .= mkScoresList pn ph viewList
  | otherwise = do 
      scorePageList .= mkScoresList pn ph scorePages 

highScores :: IO ()
highScores = do
  chan <- newBChan 256
  _ <- forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay (1*10^5)
  -- scoreArray <- V.fromList <$> withConnection dbPath (getPages (PageNumber 0) (PageHeight defHeight)) -- Local test func
  scoreArray <- V.fromList <$> leaderBoardRequest (PageNumber 0) (PageHeight defHeight)
  let initialIndex = ViewReplayForm 1
  (_, v) <- customMainWithDefaultVty
      Nothing
      highScoresApp
      ( HighScoreState
          (PageNumber 0)
          (PageHeight defHeight)
          scoreArray
          (mkScoresList (PageNumber 0) (PageHeight defHeight) scoreArray)
          defShowAmountStateDialog
          (selectReplayForm initialIndex)
          Page
      )
  V.shutdown v
