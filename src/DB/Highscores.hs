{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module DB.Highscores where

import Data.Foldable (forM_)
import Data.List.NonEmpty (nonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word
import Database.SQLite.Simple
import GameLogic (ScoreType)
import DB.Types

maxDbSize :: Word8 -- 255
maxDbSize = maxBound

openDatabase :: String -> IO Connection
openDatabase path = do
  conn <- open path
  execute_ conn initQuery
  pure conn

initQuery :: Query
initQuery = Query "CREATE TABLE IF NOT EXISTS scores\ 
                            \(id INTEGER PRIMARY KEY, name TEXT NOT NULL,\
                            \score NUMBER NOT NULL,\ 
                            \time NUMBER NOT NULL\
                            \replay BLOB\
                            \);"

scoreQuery :: Query
scoreQuery = Query "SELECT name, score, time FROM scores ORDER BY score DESC, time DESC LIMIT (?);"

addQuery :: Query
addQuery = Query "INSERT INTO scores (name, score, time) VALUES (?,?,?);"

pruneQuery :: Query
pruneQuery = Query "DELETE from scores where id not in SELECT name, score, time FROM scores ORDER BY score DESC, time DESC LIMIT (?);"

getScores :: Connection -> IO [ScoreField]
getScores conn = query conn scoreQuery (Only maxDbSize)

debugPrintScores :: Connection -> IO ()
debugPrintScores conn = do
  scores <- getScores conn
  forM_ scores $ \(ScoreField n s d _) ->
    putStrLn $
      show (n :: Name)
        <> " "
        <> show (s :: Score)
        <> " "
        <> show (d :: Int)

addScore :: Connection -> Name -> Score -> Time -> IO ()
addScore conn name score time = do
  execute conn addQuery (T.toUpper . T.take 3 $ name, score, time)

lowestScoreFromScoreList :: [ScoreField] -> Maybe Score

-- | Gets the maxDbSize'th score from the array of scores. If there is no score, then we return Nothing.
--    Otherwise, return the score, wrapped in a Just.
lowestScoreFromScoreList scores =
  case NE.drop (fromIntegral maxDbSize - 1) <$> nonEmpty scores of
    Nothing -> Nothing
    Just xs -> getScoreFieldScore . NE.head <$> nonEmpty xs

promptAddHighScore :: Connection -> Score -> IO Bool
promptAddHighScore conn s = do
  scores <- getScores conn
  case lowestScoreFromScoreList scores of
    Nothing -> return (s > 0) -- Don't add 0 scores
    Just s' -> return (s > s')

pruneAfterDbSize :: Connection -> IO ()
pruneAfterDbSize = pruneAfter $ fromIntegral maxDbSize
  where
    pruneAfter :: Int -> Connection -> IO ()
    pruneAfter num conn = execute conn pruneQuery (Only num)

testDb :: IO ()
testDb = do
  db <- openDatabase "highscores.db"
  t <- floor <$> getPOSIXTime
  addScore db "Richard" 15 t
  addScore db "Thomas" 40 t
  addScore db "Henry" 65 t
  addScore db "Alice" 80 t
  addScore db "Bamidele" 100 t

--   printScores db
