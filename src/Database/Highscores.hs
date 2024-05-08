{-# LANGUAGE OverloadedStrings #-}
module Database.Highscores where

import Database.SQLite.Simple
import Data.Time
import Data.Text (Text)
import qualified Data.Text as T 
import Data.Foldable (forM_)
import Database.SQLite.Simple.FromField (FromField, fromField)

type Score = Int
type Name = Text
type Time = UTCTime

data ScoreField = ScoreField Name Score Time

instance FromRow ScoreField where
  fromRow = ScoreField <$> field <*> field <*> field


instance ToRow ScoreField where
  toRow (ScoreField name score time) = 
    toRow (name, score, time)


scoreQuery = Query "Select TOP 10 name, score, time FROM scores ORDER BY score DESC"

getScores :: Connection -> IO [ScoreField]
getScores conn = query_ conn scoreQuery

printScores conn = do
  scores <- getScores conn
  forM_ scores $ \(ScoreField n s d) ->
    putStrLn $ show (n :: Name) <>
      " " <> show (s :: Int) <> show (d :: UTCTime)

addScore conn name score time = undefined