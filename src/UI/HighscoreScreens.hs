{-# LANGUAGE BangPatterns #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module UI.HighscoreScreens where

import Brick
import qualified Brick as B
import qualified Brick.AttrMap as A
import Brick.BChan
import Brick.Forms
import Brick.Main as M
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Dialog (Dialog)
import qualified Brick.Widgets.Dialog as D
import qualified Brick.Widgets.List as L
import Control.Concurrent (forkIO, threadDelay)
import qualified Control.Exception as E
import Control.Monad (forever, join, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import DB.Client (leaderBoardRequest, recvReplayData, heartbeatRequest)
import DB.Highscores
  ( dbPath,
    getReplayData,
    getScoreSlice,
    maxDbSize,
  )
import DB.Types (PageHeight (..), PageNumber (..), ScoreField (..), ServerStateError)
import Data.Coerce (coerce)
import Data.List (intersperse)
import Data.Maybe (fromJust, isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time
import qualified Data.Vector as V
import Data.Word (Word8)
import Database.SQLite.Simple (withConnection)
import qualified Database.SQLite.Simple as DB
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as V
import Lens.Micro ((^.))
import Lens.Micro.Mtl (use, (%=), (.=), (<~))
import Lens.Micro.TH (makeLenses)
import qualified Network.Wreq.Session as WreqS
import Options.Options (getOpts, online)
import qualified Options.Options as Opts
import UI.ReplayPlayer (replayFromReplayData)
import UI.Types (Tick (..))

data HSPageName = ScoreTable | HSDialogNum Int | ReplayIndex | InvalidIndex | ContinueConnect ConnectOpts
  deriving (Show, Eq, Ord)

data ConnectOpts = Retry | Local | Disconnect
  deriving (Show, Eq, Ord, Enum)

data Mode
  = Page
  | ShowingAmountStateDialog
  | ShowingViewReplayDialog
  | ReloadingScores
  | FetchingScores
  | ConnectPrompt

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
    _connectPrompt :: Dialog ConnectOpts HSPageName,
    _options :: !Opts.Options,
    _mode :: !Mode,
    _sess :: !WreqS.Session
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
  Page -> [debugStats, scores]
  FetchingScores -> [debugStats, fetching, scores]
  ReloadingScores -> [debugStats, reload, scores]
  ShowingViewReplayDialog -> [formWidget, scores]
    where
      formWidget =
        C.centerLayer . B.joinBorders . B.border . B.vLimit 25 . B.hLimit 75 $
          vBox [renderForm (hss ^. selectReplay), B.hBorderWithLabel (txt "Controls"), formControls]
  ShowingAmountStateDialog ->
    [diaWidget (hss ^. selectAmount), scores]
  ConnectPrompt ->
    [debugStats, connectPromptWidget, scores]
  where
    scores =
      allList (hss ^. pageHeight) (hss ^. pageNumber) (hss ^. (options . online)) (hss ^. scorePageList)
        <+> controls
    diaWidget (ShowAmountStateDialog dia) = C.centerLayer . B.vLimit 10 . B.hLimit 60 . B.joinBorders . B.freezeBorders . vBox $ [D.renderDialog dia emptyWidget, diaControls]
      where
        diaControls = C.hCenter . B.hLimit 40 . B.borderWithLabel (txt "Controls") . hBox . fmap C.hCenter $ [txt "\x2190: Left", txt "\x2192: Right", txt "\x21b2: Select"]
    connectPromptWidget = D.renderDialog (hss ^. connectPrompt) emptyWidget
    fetching = C.centerLayer . B.border $ str "Fetching scores..."
    reload = C.centerLayer $ B.border $ str "Reloading scores..."
    formControls =
      B.joinBorders . hBox . fmap C.hCenter $
        [ txt "0-9: Enter numbers",
          B.vBorder,
          txt "\x21b2: Submit",
          B.vBorder,
          txt "q: Exit"
        ]
    controls =
      C.vCenter . B.hLimit 44 . B.joinBorders . B.borderWithLabel (txt "Controls") $
        vBox $
          intersperse
            B.hBorder
            [ txt "\x2191 / \x2193: Previous / Next Record",
              txt "\x2190 / \x2192: Previous / Next Page",
              txt "h: Change number of records per page",
              txt "r: Refresh scores from the database",
              txt "c: Toggle Online/Offline leaderboards",
              txt "/: Search for replay at index",
              txt "\x21b2: View current replay",
              txt "q, ESC: Leave page"
            ]
    debugStats =
      vBox $
        [ str $ "PageNumber " ++ show (hss ^. pageNumber),
          str $ "PageHeight " ++ show (hss ^. pageHeight),
          str $ "ScoreArrIxs" ++ show (fst <$> hss ^. scoreArr),
          str $ "Online: " ++ show (hss ^. (options . online))
        ]

mkScoresList :: PageNumber -> PageHeight -> V.Vector (Int, ScoreField) -> L.List HSPageName (Int, ScoreField)
mkScoresList (PageNumber pn) (PageHeight ph) scores = L.list ScoreTable (V.take ph . V.dropWhile ((/= ph * pn) . fst) $ scores) 2

allList :: (Traversable t, L.Splittable t, Ord n, Show n) => PageHeight -> PageNumber -> Bool -> L.GenericList n t (Int, ScoreField) -> Widget n
allList pHeight pNum scoreList onl =
  C.centerLayer . vBox . (hLimit 75 . B.border <$>) $
    [ C.hCenter . withAttr headerAttr $ txt "HIGH SCORES",
      renderScoresList pHeight pNum scoreList onl
    ]

renderScoresList :: (Traversable t, L.Splittable t, Ord n, Show n) => PageHeight -> PageNumber -> Bool -> L.GenericList n t (Int, ScoreField) -> Widget n
renderScoresList pHeight pNum onl l =
  B.joinBorders . B.freezeBorders . vBox $
    [ vLimit (2 * coerce pHeight) . L.renderList f True $ l,
      B.hBorder,
      C.hCenter . hBox . fmap (padRight (Pad 3)) $ [str ("Page: " ++ show (pNum + 1)), str ("Online: " ++ show onl)]
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
inputHandler (AppEvent Tick) = do
  onl <- use (options . online)
  !heartbeat <- if not onl then pure False else liftIO . heartbeatRequest =<< use sess
  let !connectionLost = (&&) onl (not heartbeat)
  when onl $
    if connectionLost
      then do
        options . online .= False
        mode .= ConnectPrompt
      else pure ()
inputHandler ev = do
  !m <- use mode
  case m of
    ShowingViewReplayDialog -> handleViewReplayForm ev
    ShowingAmountStateDialog -> handleEventDialog ev
    Page -> handleEventMain ev
    ReloadingScores -> do
      join $ changeScoreArr <$> use pageNumber <*> use pageHeight <*> use scoreArr <*> use sess
      mode .= Page
    FetchingScores -> pure ()
    ConnectPrompt -> join $ handleConnectPrompt <$> use pageNumber <*> use pageHeight <*> pure ev

handleConnectPrompt :: PageNumber -> PageHeight -> BrickEvent HSPageName Tick -> EventM HSPageName HighScoreState ()
handleConnectPrompt pn ph (VtyEvent (V.EvKey V.KEnter [])) = do
  d <- use connectPrompt
  case maybe Disconnect snd $ D.dialogSelection d of
    Disconnect -> M.halt
    Retry -> do
      (options . online) .= True
      attemptFetchScores pn ph =<< use sess
    Local -> do
      (options . online) .= False
      fetchLocalScores pn ph
handleConnectPrompt _ _ (VtyEvent ev) =
  zoom connectPrompt $ D.handleDialogEvent ev
handleConnectPrompt _ _ _ = pure ()

attemptFetchScores :: PageNumber -> PageHeight -> WreqS.Session -> EventM n HighScoreState ()
attemptFetchScores pn ph session = do
  mode .= FetchingScores
  res <- liftIO $ fmap V.fromList <$> E.try (leaderBoardRequest pn ph session)
  either
    (\(_ :: ServerStateError) -> mode .= ConnectPrompt)
    rest
    res
  where
    rest v = do
      scoreArr .= v
      updateScorePageList
      mode .= Page

fetchLocalScores :: PageNumber -> PageHeight -> EventM n HighScoreState ()
fetchLocalScores pn ph = do
  res <- liftIO . withConnection dbPath $ getLocalPages pn ph
  scoreArr .= (V.fromList res)
  updateScorePageList
  mode .= Page

updateScorePageList :: EventM n HighScoreState ()
updateScorePageList = do
  l <- mkScoresList <$> use pageNumber <*> use pageHeight <*> use scoreArr
  scorePageList .= l

handleEventMain :: BrickEvent HSPageName Tick -> EventM HSPageName (HighScoreState) ()
handleEventMain (VtyEvent (V.EvKey (V.KChar 'q') [])) = M.halt
handleEventMain (VtyEvent (V.EvKey V.KEsc [])) = M.halt
handleEventMain (VtyEvent (V.EvKey (V.KChar 'r') [])) = do
  mode .= ReloadingScores
handleEventMain (VtyEvent (V.EvKey (V.KChar 'c') [])) = do
  options %= Opts.toggleOnline
  mode .= ReloadingScores
handleEventMain (VtyEvent (V.EvKey (V.KChar '/') [])) = do
  mode .= ShowingViewReplayDialog
handleEventMain (VtyEvent (V.EvKey (V.KChar 'h') [])) = do
  mode .= ShowingAmountStateDialog
  selectAmount .= defShowAmountStateDialog
handleEventMain (VtyEvent (V.EvKey V.KLeft [])) = do
  pageNumber %= (\(PageNumber !n) -> PageNumber (max 0 (n - 1)))
  join $ changeScoreArr <$> use pageNumber <*> use pageHeight <*> use scoreArr <*> use sess
handleEventMain (VtyEvent (V.EvKey V.KRight [])) = do
  (PageHeight hei) <- use pageHeight
  pageNumber %= (\(PageNumber !n) -> PageNumber (min ((fromIntegral maxDbSize) `div` hei) (n + 1)))
  join $ changeScoreArr <$> use pageNumber <*> use pageHeight <*> use scoreArr <*> use sess
handleEventMain (VtyEvent ev) = zoom scorePageList $ L.handleListEvent ev
handleEventMain _ = mode <~ use mode

handleEventDialog :: BrickEvent HSPageName Tick -> EventM HSPageName (HighScoreState) ()
handleEventDialog (VtyEvent (V.EvKey V.KEnter [])) = do
  d <- use (selectAmount . menuDialog)
  pageHeight .= (PageHeight . maybe defHeight snd . D.dialogSelection $ d)
  pageNumber .= 0
  ph <- use pageHeight
  s <- liftIO $ V.fromList <$> withConnection dbPath (getLocalPages (PageNumber 0) ph)
  scoreArr .= s
  list <- mkScoresList <$> use pageNumber <*> use pageHeight <*> use scoreArr
  scorePageList .= list
  mode .= Page
handleEventDialog (VtyEvent (V.EvKey V.KEsc [])) = do
  mode .= Page
handleEventDialog (VtyEvent (V.EvKey (V.KChar 'q') [])) = do
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
  onl <- checkOnline
  let (ViewReplayForm index) = formState f
      mbScoreField = snd <$> V.find ((== index - 1) . fromIntegral . fst) scores -- TODO: fix this so that we work with 1 indexes exclusively on this side
  if not . isJust $ (getReplay =<< mbScoreField)
    then do
      selectReplay .= setFieldValid False ReplayIndex f
      mode .= ShowingViewReplayDialog
    else do
      let scoreID = getScoreFieldID $ fromJust mbScoreField
      selectReplay .= setFieldValid True ReplayIndex f
      mbReplay <-
        liftIO $
          if onl
            then (recvReplayData scoreID)
            else withConnection "highscores.db" $ \dbConn -> getReplayData scoreID dbConn
      suspendAndResume' $ mapM_ (liftIO . replayFromReplayData) mbReplay
      mode .= Page
handleViewReplayForm ev = zoom selectReplay $ handleFormEvent ev

checkOnline :: EventM n HighScoreState Bool
checkOnline = use (options . online) >>= \b -> if b then liftIO =<< heartbeatRequest <$> use sess else pure b

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
    fields = [heading @@= editShowableFieldWithValidate replayIndex ReplayIndex validations]
    validations :: Word8 -> Bool
    validations x = all ($ x) [(>= 1), (<= maxDbSize) . fromIntegral]
    heading = (<=>) (padBottom (Pad 1) . C.hCenter $ withAttr headerAttr $ txt "Select the index of the replay to watch: ")

connectDialog :: Dialog ConnectOpts HSPageName
connectDialog =
  D.dialog
    (Just $ txt "Could not connect to server. Retry connection?")
    (Just (ContinueConnect Retry, connectOpts))
    60
  where
    connectOpts =
      [ ("Yes", ContinueConnect Retry, Retry),
        ("Local", ContinueConnect Local, Local),
        ("No", ContinueConnect Disconnect, Disconnect)
      ]

defDialog :: Dialog Int HSPageName
defDialog =
  D.dialog
    (Just $ txt "How many scores to show per page?")
    (Just (HSDialogNum defHeight, scoreOpts))
    60
  where
    scoreOpts = (\n -> (show n, HSDialogNum n, n)) <$> [5, 10, 25]

defShowAmountStateDialog :: ShowAmountStateDialog
defShowAmountStateDialog = ShowAmountStateDialog defDialog

getLocalPages :: PageNumber -> PageHeight -> DB.Connection -> IO [(Int, ScoreField)]
getLocalPages (PageNumber n) (PageHeight h) dbConn = do
  let indices = concatMap scoreIx pns
  zip indices <$> getScoreSlice (length indices) (PageNumber n) (PageHeight h) dbConn
  where
    scoreIx pn = [pn * h .. pn * h + h - 1]
    pns = [max 0 (n - 2) .. n + 2]

changeScoreArr :: PageNumber -> PageHeight -> V.Vector (Int, ScoreField) -> WreqS.Session -> EventM n (HighScoreState) ()
changeScoreArr pn@(PageNumber pNum) ph@(PageHeight pHei) scorePages session
  | Nothing <- V.find (== (pNum * pHei + 1)) (fst <$> scorePages) = do
      mode .= FetchingScores
      onl <- checkOnline
      if onl
        then attemptFetchScores pn ph session
        else fetchLocalScores pn ph
  | otherwise = do
      scorePageList .= mkScoresList pn ph scorePages

highScores :: V.Vty -> WreqS.Session -> IO V.Vty
highScores vty session = do
  chan <- newBChan 64
  opts <- getOpts
  let onl = opts ^. online
      defPage = PageNumber 0
      defPHeight = PageHeight defHeight
  heartbeat <- if onl then heartbeatRequest session else pure False
  _ <- forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay (1 * 10 ^ (6 :: Int))
  eScoreArray <-
    if heartbeat
      then E.try $ V.fromList <$> leaderBoardRequest defPage defPHeight session
      else Right . V.fromList <$> withConnection dbPath (getLocalPages defPage defPHeight)
  let scoreArray = (either (\(_ :: ServerStateError) -> V.empty) id eScoreArray)
      initialIndex = ViewReplayForm 1
  (gs, vty') <-
    customMainWithVty
      vty
      (V.mkVty V.defaultConfig)
      (Just chan)
      highScoresApp
      ( HighScoreState
          defPage
          defPHeight
          scoreArray
          (mkScoresList defPage defPHeight scoreArray)
          defShowAmountStateDialog
          (selectReplayForm initialIndex)
          connectDialog
          opts
          (if (heartbeat || not onl) then Page else ConnectPrompt)
          session
      )
  _ <- Opts.saveOpts (gs ^. options)
  pure vty'