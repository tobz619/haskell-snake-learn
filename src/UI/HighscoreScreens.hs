{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module UI.HighscoreScreens where

import DB.Highscores

import Lens.Micro
import Lens.Micro.TH
import Lens.Micro.Extras
import Lens.Micro.Extras (view)
import Lens.Micro.Mtl (use)

import Brick
import Brick.Widgets.Table
import Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.AttrMap as A
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty.CrossPlatform as V
import qualified Graphics.Vty as V



import Data.Text(Text)
import qualified Data.Text as Text
import Data.List (mapAccumL)
import Data.Time
import UI.MainMenu(runMainMenu)
import Brick.Widgets.Dialog (Dialog, dialog, dialogSelection)
import Data.Maybe (fromMaybe)

data ScoreTable = ScoreTable
  deriving (Show, Eq, Ord)

data MenuState = MenuState { _menuDialog :: Dialog Int Int
                           , _menuChoice :: Int 
                           }

data HighScoreState = HighScoreState { _highscores :: ![ScoreField]
                                     , _height :: !Int
                                     , _selectScore :: MenuState
                                     }
concat <$> mapM makeLenses [''HighScoreState, ''MenuState]

defHeight :: Int
defHeight = 20

ui :: HighScoreState -> [Widget ScoreTable]
ui hss = [C.center $ allTable (view height hss) (scoresTable (view highscores hss))]

scoresTable :: [ScoreField] -> Table ScoreTable
scoresTable scores =

  let scoreTable = mapAccumL mkIndex (1 :: Integer) scores

      mkIndex num s = (num+1 ,[txt . Text.pack . show $ num] <> handleScoreField s)

      handleScoreField (ScoreField n s d) = map (padLeftRight 3) [txt n, handleScore s, handleDate d]

      handleScore = txt . Text.pack . show

      handleDate = txt . formatDbIntToTime

   in surroundingBorder False $ table . snd $ scoreTable


tableVpScroll :: ViewportScroll ScoreTable
tableVpScroll = M.viewportScroll ScoreTable

allTable :: Int -> Table ScoreTable -> Widget ScoreTable
allTable h scoreTab = renderTable
            $ setDefaultColAlignment AlignCenter
            $ table [ [withAttr headerAttr 
                       $ hLimit 40 
                       $ txtWrap "               HIGH SCORES"]
                    , [ setAvailableSize (40,h) 
                        $ viewport ScoreTable Vertical 
                        $ renderTable 
                        $ columnBorders False 
                        scoreTab
                      ]
                    ]

formatDbIntToTime :: Int -> Text
formatDbIntToTime posixTime = let
  !utcTime = secondsToNominalDiffTime (fromIntegral posixTime) `addUTCTime` UTCTime (ModifiedJulianDay 0) 0 -- Convert to UTC Time

  in Text.pack $ formatTime defaultTimeLocale "%T %u %b" utcTime

inputHandler :: BrickEvent ScoreTable e -> EventM ScoreTable HighScoreState ()
inputHandler (T.VtyEvent (V.EvKey V.KDown  [])) = M.vScrollBy tableVpScroll 2
inputHandler (T.VtyEvent (V.EvKey V.KUp    [])) = M.vScrollBy tableVpScroll (-2)
inputHandler (T.VtyEvent (V.EvKey V.KLeft  [])) = M.vScrollBy tableVpScroll (-20)
inputHandler (T.VtyEvent (V.EvKey V.KRight [])) = M.vScrollBy tableVpScroll 20 
inputHandler (T.VtyEvent (V.EvKey V.KEsc   [])) = M.halt
inputHandler _ = return ()
inputHandler (T.VtyEvent (V.EvKey (V.KChar 'A') [])) = do
  dia <- use (selectScore.menuDialog)
  let res = fromMaybe (10,10) $ dialogSelection dia
  return ()


theMap :: AttrMap
theMap = A.attrMap V.defAttr
    [ (headerAttr,  fg V.white)
    , (cellAttr  ,  V.red `on` V.white)
    , (bgAttr    ,  bg V.red)
    ]

headerAttr, cellAttr, bgAttr :: AttrName
headerAttr = bgAttr <> attrName "header"
cellAttr   = attrName "cell"
bgAttr     = attrName "bg"

highScoresApp :: App HighScoreState e ScoreTable
highScoresApp = M.App { M.appDraw = ui
                        , M.appChooseCursor = M.neverShowCursor
                        , M.appHandleEvent = inputHandler
                        , M.appStartEvent = return ()
                        , M.appAttrMap = const theMap
                        }


selDialog :: Dialog Int Int
selDialog = dialog (Just $ txtWrap "How many scores to show per page")
                   (Just (defHeight `div` 2, options))
                   125
  where options = [ ("5", 10, 10)
                  , ("10", 20, 20)
                  , ("25", 50, 50)
                  , ("50", 100,100)
                  ]


defMenuState :: MenuState
defMenuState = MenuState selDialog defHeight

highScores :: IO ()
highScores = do
  db <- openDatabase "highscores.db"
  scores <- getScores db
  _ <- defaultMain highScoresApp (HighScoreState scores defHeight defMenuState)
  return ()
