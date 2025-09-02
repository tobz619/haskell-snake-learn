{-# LANGUAGE BangPatterns #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module UI.HighscoreScreens where

import Brick
import qualified Brick.AttrMap as A
import Brick.Forms
import Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Dialog (Dialog)
import qualified Brick.Widgets.Dialog as D
import qualified Brick.Widgets.List as L
import Control.Concurrent.Async
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import DB.Client (recvReplayData)
import DB.Highscores
  ( getReplayData,
    getScores,
    maxDbSize,
    openDatabase,
  )
import DB.Types (ScoreField (..))
import Data.Char (chr, isNumber)
import Data.List (foldl', mapAccumL, (!?))
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time
import qualified Data.Vector as V
import Data.Word (Word8)
import Database.SQLite.Simple (Connection)
import qualified Graphics.Vty as V
import Lens.Micro ((^.))
import Lens.Micro.Extras (view)
import Lens.Micro.Mtl (preview, use, (.=))
import Lens.Micro.TH (makeLenses)
import UI.ReplayPlayer
import Graphics.Vty.Input.Events (Event)
import qualified Brick.Widgets.Border as B

data HSPageName = ScoreTable | HSDialogNum Int | ReplayIndex | InvalidIndex
  deriving (Show, Eq, Ord)

data Mode = Page | ShowingAmountStateDialog | ShowingViewReplayDialog

newtype ShowAmountStateDialog = ShowAmountStateDialog
  { _menuDialog :: Dialog Int HSPageName
  }

newtype ViewReplayForm = ViewReplayForm
  { _replayIndex :: Word8
  }
  deriving newtype (Show, Read, Num)

data HighScoreState e = HighScoreState
  { _conn :: Connection,
    _highscores :: L.List HSPageName ScoreField,
    _height :: !Int,
    _selectScore :: ShowAmountStateDialog,
    _selectReplay :: Form ViewReplayForm e HSPageName,
    _mode :: Mode
  }

concat <$> mapM makeLenses [''HighScoreState, ''ShowAmountStateDialog, ''ViewReplayForm]

defHeight :: Int
defHeight = 20

ui :: HighScoreState e -> [Widget HSPageName]
ui hss = case hss ^. mode of
  Page -> [scores]
  ShowingViewReplayDialog -> [formWidget, scores]
    where
      formWidget = C.centerLayer $ renderForm (hss ^. selectReplay)
  ShowingAmountStateDialog ->
    [diaWidget (hss ^. selectScore), scores]
  where
    hei = view height hss
    scos = view highscores hss
    -- scores = allTable hei ScoreTable (scoresTable ScoreTable scos)
    scores = allList hei scos
    diaWidget (ShowAmountStateDialog dia) = D.renderDialog dia emptyWidget

scoresList :: [ScoreField] -> L.List HSPageName ScoreField
scoresList scores = L.list ScoreTable (V.fromList scores) 2

renderScoresList ::
  (Traversable t, L.Splittable t, Ord n, Show n) => Bool -> L.GenericList n t ScoreField -> Widget n
renderScoresList = L.renderListWithIndex f
  where
    f ix sel s
      | sel = withAttr D.buttonAttr $ lister ix s
      | otherwise = lister ix s
    lister ix (ScoreField _ n s d _ rep) =
        ( \btn ->
            hBox $ padding <$>
              [ str (show $ ix + 1),
                txt n,
                handleScore s,
                handleDate d,
                btn
              ]
        )
          (maybe emptyWidget (const replayButton) rep)

    padding = padLeftRight 2

    handleScore = txt . Text.pack . show

    handleDate = txt . formatDbIntToTime

    replayButton = withAttr headerAttr $ txt "VIEW"

allList :: (Traversable t, L.Splittable t, Ord n, Show n) => Int -> L.GenericList n t ScoreField -> Widget n
allList h scoreList =
  C.center . vBox . (hLimit 50 . B.border <$>) $
    [ C.hCenter . withAttr headerAttr $ txt "HIGH SCORES",
      vLimit h . renderScoresList True $ scoreList
    ]

formatDbIntToTime :: Int -> Text
formatDbIntToTime posixTime =
  let !utcTime = secondsToNominalDiffTime (fromIntegral posixTime) `addUTCTime` UTCTime (ModifiedJulianDay 0) 0 -- Convert to UTC Time
   in Text.pack $ formatTime (defaultTimeLocale {knownTimeZones = [read "GMT"]}) "%T %u %b" utcTime

inputHandler :: BrickEvent HSPageName e -> EventM HSPageName (HighScoreState e) ()
inputHandler ev = do
  !m <- use mode
  case m of
    ShowingViewReplayDialog -> handleViewReplayForm ev
    ShowingAmountStateDialog -> handleEventDialog ev
    Page -> handleEventMain ev

handleEventMain :: BrickEvent HSPageName e -> EventM HSPageName (HighScoreState e) ()
handleEventMain (VtyEvent (V.EvKey V.KEsc [])) = M.halt
handleEventMain (VtyEvent (V.EvKey (V.KChar 'q') [])) = M.halt
handleEventMain (VtyEvent (V.EvKey (V.KChar '/') [])) = do
  mode .= ShowingViewReplayDialog
handleEventMain (VtyEvent (V.EvKey (V.KChar 'h') [])) = do
  mode .= ShowingAmountStateDialog
  selectScore .= defShowAmountStateDialog
handleEventMain (VtyEvent ev) = zoom highscores $ L.handleListEvent ev
handleEventMain _ = return ()

handleEventDialog :: BrickEvent HSPageName e -> EventM HSPageName (HighScoreState e) ()
handleEventDialog (VtyEvent (V.EvKey V.KEnter [])) = do
  d <- fromMaybe defDialog . preview (selectScore . menuDialog) <$> get
  height .= (maybe defHeight snd . D.dialogSelection $ d)
  mode .= Page
handleEventDialog (VtyEvent (V.EvKey V.KEsc [])) = do
  mode .= Page
handleEventDialog (VtyEvent ev) = zoom (selectScore . menuDialog) $ D.handleDialogEvent ev
handleEventDialog _ = return ()

handleViewReplayForm :: BrickEvent HSPageName e -> EventM HSPageName (HighScoreState e) ()
handleViewReplayForm (VtyEvent (V.EvKey (V.KChar 'q') [])) = mode .= Page
handleViewReplayForm (VtyEvent (V.EvKey V.KEnter [])) = do
  f <- use selectReplay
  c <- use conn
  scores <- use (highscores . L.listElementsL)
  let (ViewReplayForm index) = formState f
      mbScoreField = scores V.!? (fromIntegral index - 1)
  if isJust (getReplay =<< mbScoreField)
    then do
      let scoreID = getScoreFieldID $ fromJust mbScoreField
      selectReplay .= setFieldValid True ReplayIndex f
      mbReplay <- liftIO (recvReplayData scoreID)
      suspendAndResume' $ mapM_ (liftIO . replayFromReplayData) mbReplay
      mode .= Page
    else do
      selectReplay .= setFieldValid False ReplayIndex f
      mode .= ShowingViewReplayDialog
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

highScoresApp :: App (HighScoreState e) e HSPageName
highScoresApp =
  M.App
    { M.appDraw = ui,
      M.appChooseCursor = M.neverShowCursor,
      M.appHandleEvent = inputHandler,
      M.appStartEvent = return (),
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
    (Just (HSDialogNum 10, options))
    60
  where
    options = (\n -> (show n, HSDialogNum (n * 2), n * 2)) <$> [5, 10, 25]

defShowAmountStateDialog :: ShowAmountStateDialog
defShowAmountStateDialog = ShowAmountStateDialog defDialog

-- refreshScreen currentIx

highScores :: IO ()
highScores = do
  db <- openDatabase "highscores.db"
  scores <- getScores db
  let initialIndex = ViewReplayForm 1
  _ <- defaultMain highScoresApp (HighScoreState db (scoresList scores) defHeight defShowAmountStateDialog (selectReplayForm initialIndex) Page)
  return ()
