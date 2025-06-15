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
import Data.List (mapAccumL)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time
  ( Day (ModifiedJulianDay),
    TimeLocale (knownTimeZones),
    UTCTime (UTCTime),
    addUTCTime,
    defaultTimeLocale,
    formatTime,
    secondsToNominalDiffTime,
  )
import qualified Graphics.Vty as V
import Lens.Micro ((^.))
import Lens.Micro.Extras (view)
import Lens.Micro.Mtl (preview, use, (.=))
import Lens.Micro.TH (makeLenses)
import Brick.Forms
import Data.Word
import Data.Char (chr, isNumber)
import Control.Monad (when)
import Database.SQLite.Simple (Connection)
import UI.ReplayPlayer (replayPlayerApp)
import Control.Monad.IO.Class (MonadIO(liftIO))

data HSPageName = ScoreTable | HSDialogNum Int | ReplayIndex
  deriving (Show, Eq, Ord)

data Mode = Page | ShowingAmountStateDialog | ShowingViewReplayDialog | InvalidIndex

newtype ShowAmountStateDialog = ShowAmountStateDialog
  { _menuDialog :: Dialog Int HSPageName
  }

newtype ViewReplayForm = ViewReplayForm {
    _replayIndex :: Word8
  } deriving newtype Num

data HighScoreState = HighScoreState
  { _conn :: Connection,
    _highscores :: ![ScoreField],
    _height :: !Int,
    _selectScore :: ShowAmountStateDialog,
    _selectReplay :: ViewReplayForm,
    _mode :: Mode
  }

concat <$> mapM makeLenses [''HighScoreState, ''ShowAmountStateDialog, ''ViewReplayForm]

defHeight :: Int
defHeight = 20

ui :: HighScoreState -> [Widget HSPageName]
ui hss = case hss ^. mode of

  Page -> [scores]

  ShowingViewReplayDialog -> [renderForm (selectReplayForm (view selectReplay hss)), scores]

  ShowingAmountStateDialog ->
    [diaWidget (view selectScore hss),
      scores]
  where
    hei = view height hss
    scos = view highscores hss
    scores = allTable hei ScoreTable (scoresTable ScoreTable scos)

diaWidget :: ShowAmountStateDialog -> Widget HSPageName
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

inputHandler :: BrickEvent HSPageName e -> EventM HSPageName HighScoreState ()
inputHandler ev = do
  !m <- use mode
  case m of
    InvalidIndex -> undefined -- todo
    ShowingViewReplayDialog -> handleViewReplayForm ev
    ShowingAmountStateDialog -> handleEventDialog ev
    Page -> handleEventMain ev

handleEventMain :: BrickEvent HSPageName e -> EventM HSPageName HighScoreState ()
handleEventMain (VtyEvent (V.EvKey V.KDown [])) = M.vScrollBy tableVpScroll 2
handleEventMain (VtyEvent (V.EvKey V.KUp [])) = M.vScrollBy tableVpScroll (-2)
handleEventMain (VtyEvent (V.EvKey V.KLeft [])) = M.vScrollBy tableVpScroll . negate =<< use height
handleEventMain (VtyEvent (V.EvKey V.KRight [])) = M.vScrollBy tableVpScroll =<< use height
handleEventMain (VtyEvent (V.EvKey V.KEsc [])) = M.halt
handleEventMain (VtyEvent (V.EvKey (V.KChar 'q') [])) = M.halt
handleEventMain (VtyEvent (V.EvKey (V.KChar 'h') [])) = do
  mode .= ShowingAmountStateDialog
  selectScore .= defShowAmountStateDialog
handleEventMain _ = return ()

handleEventDialog :: BrickEvent HSPageName e -> EventM HSPageName HighScoreState ()
handleEventDialog (VtyEvent (V.EvKey V.KEnter [])) = do
  d <- fromMaybe defDialog . preview (selectScore . menuDialog) <$> get
  height .= (maybe defHeight snd . D.dialogSelection $ d)
  mode .= Page
handleEventDialog (VtyEvent (V.EvKey V.KEsc [])) = do
  mode .= Page
handleEventDialog (VtyEvent ev) = zoom (selectScore . menuDialog) $ D.handleDialogEvent ev
handleEventDialog _ = return ()

handleViewReplayForm :: BrickEvent n e -> EventM HSPageName HighScoreState ()
handleViewReplayForm (VtyEvent (V.EvKey V.KEnter [])) = do
  f <- use selectReplay
  let form = selectReplayForm f
      inv = invalidFields form
  
  if null inv 
    then replayRunner form
    else do let f' = mapM (setFieldValid False) inv  form 
            pure ()



handleViewReplayForm _ = pure ()

replayRunner :: Form ViewReplayForm e n -> EventM HSPageName HighScoreState ()
replayRunner f = do
  c <- use conn
  scores <- use highscores
  let ViewReplayForm index = formState f
      (ScoreField{getScoreFieldID}) = scores !! (fromIntegral index - 1)
  mbReplay <- liftIO $ getReplayData c getScoreFieldID
  maybe (mode .= InvalidIndex) (liftIO . replayPlayerApp) mbReplay

theMap :: AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (headerAttr, fg V.white),
      (cellAttr, V.red `on` V.white),
      (bgAttr, bg V.red),
      (D.dialogAttr, fg V.white),
      (D.buttonAttr, V.red `on` V.white),
      (D.buttonSelectedAttr, V.white `on` V.red)
    ]

headerAttr, cellAttr, bgAttr :: AttrName
headerAttr = bgAttr <> attrName "header"
cellAttr = attrName "cell"
bgAttr = attrName "bg"

highScoresApp :: App HighScoreState e HSPageName
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
  where fields = [heading @@= editShowableFieldWithValidate replayIndex ReplayIndex validations]
        validations x = all ($ x) [isNumber . chr . fromIntegral, (>= 1) . fromIntegral, (<= maxDbSize + 1) . fromIntegral]
        heading = (<=>) (padBottom (Pad 3) $ withAttr headerAttr $ txt "Select the number of the replay to watch: ")

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
  _ <- defaultMain highScoresApp (HighScoreState db scores defHeight defShowAmountStateDialog  (ViewReplayForm 0) Page)
  return ()
