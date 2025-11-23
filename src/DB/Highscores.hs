{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module DB.Highscores where

import DB.Types
import qualified Data.ByteString.Lazy as BS
import Data.Foldable (forM_)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Database.SQLite.Simple
import UI.Types ( SeedType )

maxDbSize :: DBSize -- 255
maxDbSize = maxBound

dbPath = "/home/tobioloke/brick-tutorial/highscores.db"

openDatabase :: IO Connection
openDatabase  = do
  withConnection dbPath $
    \conn -> execute_ conn initQuery >> pure conn

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

nonceInitQuery :: Query
nonceInitQuery =
  Query
    "CREATE TABLE IF NOT EXISTS sessions \
    \id INTEGER PRIMARY KEY"

scoreQuery :: Query
scoreQuery = Query "SELECT id, name, score, time, seed, replay FROM scores ORDER BY score DESC, time DESC LIMIT (?);"

sliceScoreQuery :: Query
sliceScoreQuery =
  Query
    "SELECT id, name, score, time, seed, replay FROM scores ORDER BY score DESC, time DESC \
    \LIMIT (?) OFFSET (?)"

lowestScoreQuery :: Query
lowestScoreQuery =
  Query
    "SELECT id, name, score, time, seed, replay \
    \FROM scores ORDER BY score ASC, time ASC LIMIT 1"

addQuery :: Query
addQuery = Query "INSERT INTO scores (name, score, time) VALUES (?,?,?);"

addReplayQuery :: Query
addReplayQuery = Query "INSERT INTO scores (name, score, time, seed, replay) VALUES (?,?,?,?,?);"

pruneQuery :: Query
pruneQuery = Query "DELETE from scores WHERE id not in (SELECT id FROM scores ORDER BY score DESC, time DESC LIMIT (?));"

getReplayQuery :: Query
getReplayQuery =
  Query "SELECT seed, replay FROM scores WHERE id=(?);"

getScores :: Connection -> IO [ScoreField]
getScores conn =
  fmap firstScore <$> query conn scoreQuery (Only maxDbSize)
  where
    firstScore sf@(ScoreField _ _ _ _ _ s) = sf {getReplay = BS.take 1 <$> s} -- Only fetch the first byte

getLowestScore :: Connection -> IO (Maybe ScoreField)
getLowestScore conn = do
  pruneAfterDbSize maxDbSize conn
  listToMaybe <$> query_ conn lowestScoreQuery

getScoreSlice :: PageNumber -> PageHeight -> Connection -> IO [ScoreField]
getScoreSlice (PageNumber lbIx) (PageHeight hei) conn = do
  info
  fmap firstScore <$> query conn sliceScoreQuery (totalLimit, offset)
  where
    firstScore sf@(ScoreField _ _ _ _ _ s) = sf {getReplay = BS.take 1 <$> s} -- Only fetch the first byte
    totalLimit = 5 * hei
    offset = (max 0 (lbIx - 2)) * hei * 5
    info = do
      putStrLn $ "Hei: " <> show totalLimit
      putStrLn $ "Ix: " <> show offset

debugPrintScores :: Connection -> IO ()
debugPrintScores conn = do
  scores <- getScores conn
  forM_ scores $ \(ScoreField _ n s d _ _) ->
    putStrLn $
      show (n :: NameType)
        <> " "
        <> show (s :: Score)
        <> " "
        <> show (d :: Int)

addScore :: NameType -> Score -> Time -> Connection -> IO ()
addScore name score time conn = do
  execute conn addQuery (T.toUpper . T.take 3 $ name, score, time)

addScoreWithReplay :: NameType -> Score -> Time -> SeedType -> EventListMessage -> Connection -> IO ()
addScoreWithReplay name score time seed evList conn =
  execute conn addReplayQuery (T.toUpper . T.take 3 $ name, score, time, seed, evList)

getReplayData :: Int -> Connection -> IO (Maybe ReplayData)
getReplayData scoreID conn =
  listToMaybe <$> query conn getReplayQuery (Only scoreID)

promptAddHighScore :: Score -> Connection -> IO Bool
promptAddHighScore s conn = do
  score <- fmap getScoreFieldScore <$> getLowestScore conn
  pure $ maybe (s > 0) (s >) score

pruneAfterDbSize :: DBSize -> Connection -> IO ()
pruneAfterDbSize num conn =
  execute conn pruneQuery (Only num)

testDb :: IO ()
testDb = do
  t <- floor <$> getPOSIXTime
  withConnection dbPath $ \db -> do
    addScore "Timmy" 5 t db
    addScore "Loser" 10 t db
    addScore "Richard" 20 t db
    addScore "Thomas" 30 t db
    addScore "Henry" 40 t db
    addScore "Alice" 50 t db
    addScore "Bamidele" 60 t db
