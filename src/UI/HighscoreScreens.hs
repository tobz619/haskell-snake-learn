{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module UI.HighscoreScreens where

import Database.Highscores

import Brick
import Brick.Widgets.Table
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty.CrossPlatform as V
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import qualified Graphics.Vty as V

import Data.Text(Text)
import qualified Data.Text as Text
import Data.List (mapAccumL)

ui :: [ScoreField] -> Widget ()
ui scores = C.center $ renderTable (allTable (scoresTable scores))

scoresTable :: [ScoreField] -> Table ()
scoresTable scores =
  
  let scoreTable = mapAccumL incCount (1 :: Integer) scores
      
      incCount num s = (num+1 ,[txt . Text.pack . show $ num] <> handleScoreField s)
      
      handleScoreField (ScoreField n s d) = map (padLeftRight 5) [txt n, handleScore s, handleDate d]
      
      handleScore = txt . Text.pack . show
      
      handleDate =  txt . Text.pack . show
   
   in surroundingBorder False $ table . snd $ scoreTable


allTable s = setDefaultColAlignment AlignCenter $ table [[txt "HIGH SCORES"], [renderTable s]]

highScores :: IO ()
highScores = do
  db <- openDatabase "highscores.db"
  scores <- getScores db
  -- debugPrintScores db
  simpleMain (ui scores)
