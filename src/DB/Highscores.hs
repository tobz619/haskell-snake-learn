{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module DB.Highscores where

import DB.Types
import Data.Foldable (forM_)
import Data.List.NonEmpty (nonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Database.SQLite.Simple
import Data.List (uncons)
import Data.Maybe (listToMaybe)
import UI.Types

maxDbSize :: DBSize -- 255
maxDbSize = maxBound

openDatabase :: String -> IO Connection
openDatabase path = do
  conn <- open path
  execute_ conn initQuery
  pure conn

initQuery :: Query
initQuery =
  Query
    "CREATE TABLE IF NOT EXISTS scores\
    \ (id INTEGER PRIMARY KEY ,\
    \ name TEXT NOT NULL,\
    \ score NUMBER NOT NULL,\
    \ time NUMBER NOT NULL,\
    \ seed BIGINT,\
    \ replay BLOB\
    \);"

scoreQuery :: Query
scoreQuery = Query "SELECT id, name, score, time, seed, replay FROM scores ORDER BY score DESC, time DESC LIMIT (?);"

addQuery :: Query
addQuery = Query "INSERT INTO scores (name, score, time) VALUES (?,?,?);"

addReplayQuery :: Query
addReplayQuery = Query "INSERT INTO scores (name, score, time, seed, replay) VALUES (?,?,?,?,?);"

pruneQuery :: Query
pruneQuery = Query "DELETE from scores where id not in SELECT name, score, time FROM scores ORDER BY score DESC, time DESC LIMIT (?);"

getReplayQuery :: Query
getReplayQuery =
  Query "SELECT seed, replay FROM scores WHERE id=(?);"

getScores :: Connection -> IO [ScoreField]
getScores conn = query conn scoreQuery (Only maxDbSize)

debugPrintScores :: Connection -> IO ()
debugPrintScores conn = do
  scores <- getScores conn
  forM_ scores $ \(ScoreField _ n s d _ _) ->
    putStrLn $
      show (n :: Name)
        <> " "
        <> show (s :: Score)
        <> " "
        <> show (d :: Int)

addScore :: Connection -> Name -> Score -> Time -> IO ()
addScore conn name score time = do
  execute conn addQuery (T.toUpper . T.take 3 $ name, score, time)

addScoreWithReplay :: Connection -> Name -> Score -> Time -> SeedType -> EventListMessage -> IO ()
addScoreWithReplay conn name score time seed evList = 
  execute conn addReplayQuery (T.toUpper . T.take 3 $ name, score, time, seed, evList)

getReplayData :: Connection -> Int -> IO (Maybe ReplayData)
getReplayData conn scoreID =
  listToMaybe <$> query conn getReplayQuery (Only scoreID)

lowestScoreFromScoreList :: [ScoreField] -> Maybe Score
-- ^ Gets the maxDbSize'th score from the array of scores. If there is no score, then we return Nothing.
--    Otherwise, return the score, wrapped in a Just.
lowestScoreFromScoreList scores =
  case uncons . NE.drop (fromIntegral maxDbSize - 1) =<< nonEmpty scores of
    Nothing -> Nothing
    Just (x,_) -> Just $ getScoreFieldScore x

promptAddHighScore :: Connection -> Score -> IO Bool
promptAddHighScore conn s = do
  scores <- getScores conn
  pure $ maybe (s > 0) (s >) (lowestScoreFromScoreList scores)


pruneAfterDbSize :: Connection -> Int -> IO ()
pruneAfterDbSize conn num = 
  execute conn pruneQuery (Only num)

testDb :: IO ()
testDb = do
  db <- openDatabase "highscores.db"
  t <- floor <$> getPOSIXTime
  addScore db "Timmy" 5 t
  addScore db "Loser" 10 t
  addScore db "Richard" 20 t
  addScore db "Thomas" 30 t
  addScore db "Henry" 40 t
  addScore db "Alice" 50 t
  addScore db "Bamidele" 60 t

--   printScores db
