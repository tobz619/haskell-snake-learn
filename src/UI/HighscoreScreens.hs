{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module UI.HighscoreScreens where

import Brick
import qualified Brick.AttrMap as A
import Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Dialog (Dialog)
import qualified Brick.Widgets.Dialog as D
import Brick.Widgets.Table
import DB.Highscores
  ( getScores,
    openDatabase, maxDbSize, getReplayData,
  )
import DB.Types (ScoreField (..))
import Data.List (mapAccumL, (!?))
import Data.Maybe (fromMaybe, isJust, fromJust)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time
import qualified Graphics.Vty as V
import Lens.Micro ((^.))
import Lens.Micro.Extras (view)
import Lens.Micro.Mtl (preview, use, (.=))
import Lens.Micro.TH (makeLenses)
import Brick.Forms
import Data.Word ( Word8 )
import Data.Char (chr, isNumber)
import Database.SQLite.Simple (Connection)
import UI.ReplayPlayer
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.List (foldl')
import Control.Monad (when)
import Control.Concurrent.Async
import DB.Client (runGetReplayData)

data HSPageName = ScoreTable | HSDialogNum Int | ReplayIndex | InvalidIndex
  deriving (Show, Eq, Ord)

data Mode = Page | ShowingAmountStateDialog | ShowingViewReplayDialog

newtype ShowAmountStateDialog = ShowAmountStateDialog
  { _menuDialog :: Dialog Int HSPageName
  }

newtype ViewReplayForm = ViewReplayForm {
    _replayIndex :: Word8
  } deriving newtype (Show, Read, Num)

data HighScoreState e = HighScoreState
  { _conn :: Connection,
    _highscores :: ![ScoreField],
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
    where formWidget = C.centerLayer $ renderForm (hss ^. selectReplay)

  ShowingAmountStateDialog ->
    [diaWidget (hss ^. selectScore), scores]
  where
    hei = view height hss
    scos = view highscores hss
    scores = allTable hei ScoreTable (scoresTable ScoreTable scos)
    diaWidget (ShowAmountStateDialog dia) = D.renderDialog dia emptyWidget


scoresTable :: n -> [ScoreField] -> Table n
scoresTable _ scores =
  let scoreTable = mapAccumL mkIndex (1 :: Integer) scores

      mkIndex num s = (num + 1, [txt . Text.pack . show $ num] <> handleScoreField s)

      handleScoreField (ScoreField _ n s d _ Nothing) = padding <$> [txt n, handleScore s, handleDate d, emptyWidget]
      handleScoreField (ScoreField _ n s d _ (Just _)) = padding <$> [txt n, handleScore s, handleDate d, replayButton]

      handleScore = txt . Text.pack . show

      handleDate = txt . formatDbIntToTime

      padding = padLeftRight 2

      replayButton = withAttr D.buttonAttr $ txt "VIEW"

   in surroundingBorder True . setDefaultColAlignment AlignCenter . table . snd $ scoreTable

tableVpScroll :: ViewportScroll HSPageName
tableVpScroll = M.viewportScroll ScoreTable

allTable :: (Show n, Ord n) => Int -> n -> Table n -> Widget n
allTable h name scoreTab = C.center . vBox . (hLimit 44 <$>)  $
  [C.hCenterWith Nothing $ withAttr headerAttr $ txt "HIGH SCORES",
  (vLimit h . viewport name Vertical .
          renderTable .
            columnBorders
              False $
                scoreTab)
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
handleEventMain (VtyEvent (V.EvKey V.KDown [])) = M.vScrollBy tableVpScroll 2
handleEventMain (VtyEvent (V.EvKey V.KUp [])) = M.vScrollBy tableVpScroll (-2)
handleEventMain (VtyEvent (V.EvKey V.KLeft [])) = M.vScrollBy tableVpScroll . negate =<< use height
handleEventMain (VtyEvent (V.EvKey V.KRight [])) = M.vScrollBy tableVpScroll =<< use height
handleEventMain (VtyEvent (V.EvKey V.KEsc [])) = M.halt
handleEventMain (VtyEvent (V.EvKey (V.KChar 'q') [])) = M.halt
handleEventMain (VtyEvent (V.EvKey (V.KChar '/') [])) = do
  mode .= ShowingViewReplayDialog
handleEventMain (VtyEvent (V.EvKey (V.KChar 'h') [])) = do
  mode .= ShowingAmountStateDialog
  selectScore .= defShowAmountStateDialog
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
  scores <- use highscores
  let (ViewReplayForm index) = formState f
      mbScoreField = scores !? (fromIntegral index - 1)
  if isJust (getReplay =<< mbScoreField)
    then do
      selectReplay .= setFieldValid True ReplayIndex f
      mbReplay <- liftIO runGetReplayData
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
  where fields = [heading @@= editShowableField replayIndex ReplayIndex]
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

highScores :: IO ()
highScores = do
  db <- openDatabase "highscores.db"
  scores <- getScores db
  let initialIndex = ViewReplayForm 1
  _ <- defaultMain highScoresApp (HighScoreState db scores defHeight defShowAmountStateDialog  (selectReplayForm initialIndex) Page)
  return ()
