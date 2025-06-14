{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module UI.HighscoreScreens where

import Brick
import qualified Brick.AttrMap as A
import Brick.Main as M
  ( App
      ( App,
        appAttrMap,
        appChooseCursor,
        appDraw,
        appHandleEvent,
        appStartEvent
      ),
    ViewportScroll (vScrollBy),
    halt,
    neverShowCursor,
    viewportScroll,
  )
import qualified Brick.Types as T
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Dialog (Dialog)
import qualified Brick.Widgets.Dialog as D
import Brick.Widgets.Table
  ( ColumnAlignment (AlignCenter),
    Table,
    columnBorders,
    renderTable,
    setDefaultColAlignment,
    surroundingBorder,
    table,
  )
import DB.Highscores
  ( 
    getScores,
    openDatabase,
  )
import Data.List (mapAccumL)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time
  ( Day (ModifiedJulianDay),
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
import DB.Types (ScoreField (..))

data HSPageName = ScoreTable | HSDialogNum Int
  deriving (Show, Eq, Ord)

data Mode = Page | ShowingDialog

data MenuState = MenuState
  { _menuDialog :: Dialog Int HSPageName,
    _menuChoice :: Int
  }

data HighScoreState = HighScoreState
  { _highscores :: ![ScoreField],
    _height :: !Int,
    _selectScore :: MenuState,
    _mode :: Mode
  }

concat <$> mapM makeLenses [''HighScoreState, ''MenuState]

defHeight :: Int
defHeight = 20

ui :: HighScoreState -> [Widget HSPageName]
ui hss = case hss ^. mode of
  Page -> [C.center $ allTable hei ScoreTable (scoresTable ScoreTable scos)]
  ShowingDialog ->
    diaWidget (view selectScore hss)
      <> [C.center $ allTable hei ScoreTable (scoresTable ScoreTable scos)]
  where
    hei = view height hss
    scos = view highscores hss

diaWidget :: MenuState -> [Widget HSPageName]
diaWidget ((MenuState dia _)) = [D.renderDialog dia emptyWidget]

scoresTable :: n -> [ScoreField] -> Table n
scoresTable _ scores =
  let scoreTable = mapAccumL mkIndex (1 :: Integer) scores

      mkIndex num s = (num + 1, [txt . Text.pack . show $ num] <> handleScoreField s)

      handleScoreField (ScoreField n s d Nothing) = map (padLeftRight 3) [txt n, handleScore s, handleDate d]
      handleScoreField (ScoreField n s d (Just _)) = map (padLeftRight 3) [txt n, handleScore s, handleDate d, replayButton]

      handleScore = txt . Text.pack . show

      handleDate = txt . formatDbIntToTime
      replayButton = withAttr D.buttonAttr $ txt "VIEW"
   in surroundingBorder False $ table . snd $ scoreTable

tableVpScroll :: ViewportScroll HSPageName
tableVpScroll = M.viewportScroll ScoreTable

allTable :: (Show n, Ord n) => Int -> n -> Table n -> Widget n
allTable h name scoreTab =
  renderTable $
    setDefaultColAlignment AlignCenter $
      table
        [ [ withAttr headerAttr $
              hLimit 40 $
                txtWrap "               HIGH SCORES"
          ],
          [ setAvailableSize (40, h) $
              viewport name Vertical $
                renderTable $
                  columnBorders
                    False
                    scoreTab
          ]
        ]

formatDbIntToTime :: Int -> Text
formatDbIntToTime posixTime =
  let !utcTime = secondsToNominalDiffTime (fromIntegral posixTime) `addUTCTime` UTCTime (ModifiedJulianDay 0) 0 -- Convert to UTC Time
   in Text.pack $ formatTime defaultTimeLocale "%T %u %b" utcTime

inputHandler :: BrickEvent HSPageName e -> EventM HSPageName HighScoreState ()
inputHandler ev = do
  !m <- use mode
  case m of
    ShowingDialog -> handleEventDialog ev
    Page -> handleEventMain ev

handleEventMain :: BrickEvent HSPageName e -> EventM HSPageName HighScoreState ()
handleEventMain (T.VtyEvent (V.EvKey V.KDown [])) = M.vScrollBy tableVpScroll 2
handleEventMain (T.VtyEvent (V.EvKey V.KUp [])) = M.vScrollBy tableVpScroll (-2)
handleEventMain (T.VtyEvent (V.EvKey V.KLeft [])) = M.vScrollBy tableVpScroll . negate =<< use height
handleEventMain (T.VtyEvent (V.EvKey V.KRight [])) = M.vScrollBy tableVpScroll =<< use height
handleEventMain (T.VtyEvent (V.EvKey V.KEsc [])) = M.halt
handleEventMain (T.VtyEvent (V.EvKey (V.KChar 'q') [])) = M.halt
handleEventMain (T.VtyEvent (V.EvKey (V.KChar 'h') [])) = do
  mode .= ShowingDialog
  selectScore .= defMenuState
handleEventMain _ = return ()

handleEventDialog :: BrickEvent HSPageName e -> EventM HSPageName HighScoreState ()
handleEventDialog (T.VtyEvent (V.EvKey V.KEnter [])) = do
  d <- fromMaybe defDialog . preview (selectScore . menuDialog) <$> get
  height .= (maybe defHeight snd . D.dialogSelection $ d)
  mode .= Page
handleEventDialog (T.VtyEvent (V.EvKey V.KEsc [])) = do
  mode .= Page
handleEventDialog (T.VtyEvent ev) = zoom (selectScore . menuDialog) $ D.handleDialogEvent ev
handleEventDialog _ = return ()

theMap :: AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (headerAttr, fg V.white),
      (cellAttr, V.red `on` V.white),
      (bgAttr, bg V.red),
      (D.dialogAttr, fg V.white),
      (D.buttonAttr, V.red `on` V.white),
      (D.buttonSelectedAttr, bg V.red)
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

defDialog :: Dialog Int HSPageName
defDialog =
  D.dialog
    (Just $ txt "How many scores to show per page?")
    (Just (HSDialogNum 10, options))
    125
  where
    options = (\n -> (show n, HSDialogNum (n * 2), n * 2)) <$> [5, 10, 25]

defMenuState :: MenuState
defMenuState = MenuState defDialog defHeight

highScores :: IO ()
highScores = do
  db <- openDatabase "highscores.db"
  scores <- getScores db
  _ <- defaultMain highScoresApp (HighScoreState scores defHeight defMenuState Page)
  return ()
