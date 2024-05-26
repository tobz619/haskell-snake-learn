{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module UI.HighscoreScreens where

import DB.Highscores

import Lens.Micro
import Lens.Micro.TH
import Lens.Micro.Extras (view)
import Lens.Micro.Mtl hiding (view)

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
import Brick.Widgets.Dialog (Dialog)
import qualified Brick.Widgets.Dialog as D
import Data.Maybe (fromMaybe)



data HSPageName = ScoreTable | ScoreDialogNum Int
  deriving (Show, Eq, Ord)

data MenuState = MenuState { _menuDialog :: Dialog Int HSPageName
                           , _menuChoice :: Int
                           }

data HighScoreState = HighScoreState { _highscores :: ![ScoreField]
                                     , _height :: !Int
                                     , _selectScore :: Maybe MenuState
                                     }
concat <$> mapM makeLenses [''HighScoreState, ''MenuState]

defHeight :: Int
defHeight = 20

ui :: HighScoreState -> [Widget HSPageName]
ui hss = [perPage (view selectScore hss) $ C.center $ allTable hei (scoresTable scos)]

         where hei = view height hss
               scos = view highscores hss

perPage :: Maybe MenuState -> (Widget HSPageName -> Widget HSPageName)
perPage Nothing = seq emptyWidget
perPage (Just (MenuState dia _)) = D.renderDialog dia

scoresTable :: [ScoreField] -> Table HSPageName
scoresTable scores =

  let scoreTable = mapAccumL mkIndex (1 :: Integer) scores

      mkIndex num s = (num+1 ,[txt . Text.pack . show $ num] <> handleScoreField s)

      handleScoreField (ScoreField n s d) = map (padLeftRight 3) [txt n, handleScore s, handleDate d]

      handleScore = txt . Text.pack . show

      handleDate = txt . formatDbIntToTime

   in surroundingBorder False $ table . snd $ scoreTable


tableVpScroll :: ViewportScroll HSPageName
tableVpScroll = M.viewportScroll ScoreTable

allTable :: Int -> Table HSPageName -> Widget HSPageName
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

inputHandler :: BrickEvent HSPageName e -> EventM HSPageName HighScoreState ()
inputHandler (T.VtyEvent (V.EvKey V.KDown  [])) = M.vScrollBy tableVpScroll 2
inputHandler (T.VtyEvent (V.EvKey V.KUp    [])) = M.vScrollBy tableVpScroll (-2)
inputHandler (T.VtyEvent (V.EvKey V.KLeft  [])) = M.vScrollBy tableVpScroll (-20)
inputHandler (T.VtyEvent (V.EvKey V.KRight [])) = M.vScrollBy tableVpScroll 20
inputHandler (T.VtyEvent (V.EvKey V.KEsc   [])) = M.halt

inputHandler key@(T.VtyEvent (V.EvKey (V.KChar 'h') [])) = do
  put . set selectScore (Just defMenuState) =<< get
  zoom (selectScore . _Just) $ dialogHandler key
  -- put . set selectScore Nothing =<< get

inputHandler _ = return ()


dialogHandler :: BrickEvent HSPageName e -> EventM HSPageName MenuState ()
dialogHandler (T.VtyEvent (V.EvKey V.KEnter [])) = do
  d <- use menuDialog
  menuChoice .= (maybe defHeight snd . D.dialogSelection $ d)
  M.halt


dialogHandler (T.VtyEvent (V.EvKey V.KEsc [])) =
  M.halt

dialogHandler (T.VtyEvent ev) = zoom menuDialog $ D.handleDialogEvent ev

dialogHandler _ = return ()

theMap :: AttrMap
theMap = A.attrMap V.defAttr
    [ (headerAttr,  fg V.white)
    , (cellAttr  ,  V.red `on` V.white)
    , (bgAttr    ,  bg V.red)
    , (D.dialogAttr, V.white `on` V.red)
    , (D.buttonAttr, V.red `on` V.white)
    , (D.buttonSelectedAttr, bg V.yellow)
    ]

headerAttr, cellAttr, bgAttr :: AttrName
headerAttr = bgAttr <> attrName "header"
cellAttr   = attrName "cell"
bgAttr     = attrName "bg"

highScoresApp :: App HighScoreState e HSPageName
highScoresApp = M.App { M.appDraw = ui
                        , M.appChooseCursor = M.neverShowCursor
                        , M.appHandleEvent = inputHandler
                        , M.appStartEvent = return ()
                        , M.appAttrMap = const theMap
                        }


defDialog :: Dialog Int HSPageName
defDialog = D.dialog (Just $ txt "How many scores to show per page?")
                   (Just (ScoreDialogNum defHeight, options))
                   125
  where options = [ ("5", ScoreDialogNum 10, 10)
                  , ("10", ScoreDialogNum 20, 20)
                  , ("25", ScoreDialogNum 50, 50)
                  , ("50", ScoreDialogNum 100,100)
                  ]


defMenuState :: MenuState
defMenuState = MenuState defDialog defHeight

highScores :: IO ()
highScores = do
  db <- openDatabase "highscores.db"
  scores <- getScores db
  _ <- defaultMain highScoresApp (HighScoreState scores defHeight Nothing)
  return ()
