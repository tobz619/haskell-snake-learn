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
import Data.Time 

data ScoreTable = ScoreTable
  deriving (Show, Eq, Ord)

ui :: [ScoreField] -> Widget ScoreTable
ui scores = C.center $ allTable (scoresTable scores)

scoresTable :: [ScoreField] -> Table ScoreTable
scoresTable scores =
  
  let scoreTable = mapAccumL mkIndex (1 :: Integer) scores
      
      mkIndex num s = (num+1 ,[txt . Text.pack . show $ num] <> handleScoreField s)
      
      handleScoreField (ScoreField n s d) = map (padLeftRight 4) [txt n, handleScore s, handleDate d]
      
      handleScore = txt . Text.pack . show
      
      handleDate = txt . formatDbIntToTime
   
   in surroundingBorder False $ table . snd $ scoreTable


allTable :: Table ScoreTable -> Widget ScoreTable
allTable s = renderTable $ setDefaultColAlignment AlignCenter $ table [[txt "HIGH SCORES"], [renderTable s]]

formatDbIntToTime :: Int -> Text
formatDbIntToTime posixTime = let
  utcTime = secondsToNominalDiffTime (fromIntegral posixTime) `addUTCTime` UTCTime (ModifiedJulianDay 0) 0
  
  in Text.pack $ formatTime defaultTimeLocale "%T %u %b" utcTime


highScores :: IO ()
highScores = do
  db <- openDatabase "highscores.db"
  scores <- getScores db
  simpleMain (ui scores)
