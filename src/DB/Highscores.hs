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
import Database.SQLite.Simple

type Score = Int

type Name = Text

type Time = Int

data ScoreField = ScoreField {getScoreFieldName :: !Name, getScoreFieldScore :: !Score, getScoreFieldTime :: !Time}

instance FromRow ScoreField where
    fromRow = ScoreField <$> field <*> field <*> field

instance ToRow ScoreField where
    toRow (ScoreField name score time) =
        toRow (name, score, time)

instance Eq ScoreField where
    (ScoreField _ x _) == (ScoreField _ y _) = x == y

instance Ord ScoreField where
    (ScoreField _ s d) <= (ScoreField _ s' d') = s <= s' && d < d'

maxDbSize :: Int
maxDbSize = 200

openDatabase :: String -> IO Connection
openDatabase path = do
    conn <- open path
    execute_ conn initQuery
    pure conn

initQuery :: Query
initQuery = Query "CREATE TABLE IF NOT EXISTS scores (id INTEGER PRIMARY KEY, name TEXT NOT NULL, score NUMBER NOT NULL, time NUMBER NOT NULL);"

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
    forM_ scores $ \(ScoreField n s d) ->
        putStrLn $
            show (n :: Name)
                <> " "
                <> show (s :: Int)
                <> " "
                <> show (d :: Int)

addScore :: Connection -> Name -> Score -> Time -> IO ()
addScore conn name score time = do
    execute conn addQuery (T.toUpper . T.take 3 $ name, score, time)

lowestScoreFromScoreList :: [ScoreField] -> Maybe Int

{- | Gets the maxDbSize'th score from the array of scores. If there is no score, then we return Nothing.
    Otherwise, return the score, wrapped in a Just.
-}
lowestScoreFromScoreList scores =
    case NE.drop (maxDbSize - 1) <$> nonEmpty scores of
        Nothing -> Nothing
        Just xs -> getScoreFieldScore . NE.head <$> nonEmpty xs

promptAddHighScore :: Connection -> Score -> IO Bool
promptAddHighScore conn s = do
    scores <- getScores conn
    case lowestScoreFromScoreList scores of
        Nothing -> return (s > 0) -- Don't add 0 scores
        Just s' -> return (s > s')

pruneAfterDbSize :: Connection -> IO ()
pruneAfterDbSize = pruneAfter maxDbSize
  where
    pruneAfter :: Int -> Connection -> IO ()
    pruneAfter num conn = execute conn pruneQuery (Only num)

-- testDb :: IO ()
-- testDb = do
--   db <- openDatabase "highscores.db"
--   t <- floor <$> getPOSIXTime
--   addScore db "Richard" 34 t
--   addScore db "Thomas" 69 t
--   addScore db "Henry" 420 t
--   addScore db "Alice" 360 t
--   addScore db "XXX" 99 t
--   addScore db "Bamidele" 432 t
--   printScores db
