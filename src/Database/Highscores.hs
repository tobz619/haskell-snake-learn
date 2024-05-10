{-# LANGUAGE OverloadedStrings, BangPatterns #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Database.Highscores where

import Database.SQLite.Simple
import Data.Time.Clock.POSIX
import Data.Text (Text)
import qualified Data.Text as T
import Data.Foldable (forM_)
import Data.List.NonEmpty ( nonEmpty )
import qualified Data.List.NonEmpty as NE

type Score = Int
type Name = Text
type Time = Int

data ScoreField = ScoreField !Name !Score !Time

instance FromRow ScoreField where
  fromRow = ScoreField <$> field <*> field <*> field


instance ToRow ScoreField where
  toRow (ScoreField name score time) =
    toRow (name, score, time)

instance Eq ScoreField where
  (ScoreField _ x _) == (ScoreField _ y _) = x == y

instance Ord ScoreField  where
  (ScoreField _ s d) <= (ScoreField _ s' d') = s <= s' && d < d'

openDatabase :: String -> IO Connection
openDatabase path = do
  conn <- open path
  execute_ conn initQuery
  pure conn 

initQuery :: Query
initQuery = Query "CREATE TABLE IF NOT EXISTS scores (id INTEGER PRIMARY KEY, name TEXT NOT NULL, score NUMBER NOT NULL, time NUMBER NOT NULL);"

scoreQuery :: Query
scoreQuery = Query "SELECT name, score, time FROM scores ORDER BY score DESC LIMIT 10;"

addQuery :: Query
addQuery =  Query "INSERT INTO scores (name, score, time) VALUES (?,?,?);"

getScores :: Connection -> IO [ScoreField]
getScores conn = query_ conn scoreQuery

printScores :: Connection -> IO ()
printScores conn = do
  scores <- getScores conn
  forM_ scores $ \(ScoreField n s d) ->
    putStrLn $ show (n :: Name) <>
      " " <> show (s :: Int) <> " " <> show (d :: Int)

addScore :: Connection -> ScoreField -> IO ScoreField
addScore conn s@(ScoreField name score time) = do 
  execute conn addQuery (T.take 3 . T.toUpper $ name, score, time)
  pure s

lowestScoreFromScoreList :: [ScoreField] -> Maybe ScoreField
lowestScoreFromScoreList scores = NE.last <$> nonEmpty scores

promptAddHighScore :: Connection -> ScoreField -> IO Bool
promptAddHighScore conn s = do
  scores <- getScores conn
  if length scores < 10
    then return True
    else case lowestScoreFromScoreList scores of
            Nothing -> return True
            Just s' -> return (s > s')

-- testDb :: IO ()
-- testDb = do
--   db <- openDatabase ""
--   t <- round <$> getPOSIXTime
--   addScore db (ScoreField "Richard" 34 t)
--   addScore db (ScoreField "Thomas" 69 t)
--   addScore db (ScoreField "Henry" 420 t)
--   addScore db (ScoreField "Alice" 360 t)
--   addScore db (ScoreField "XXX" 99 t)
--   addScore db (ScoreField "Bamidele" 432 t)
--   printScores db