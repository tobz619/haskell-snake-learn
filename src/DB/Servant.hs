{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module DB.Servant where

import Control.Monad.IO.Class (liftIO)
import DB.Highscores (getScoreSlice)
import DB.Types
    ( ScoreField, PageNumber(PageNumber), PageHeight(PageHeight) )
import Data.Proxy ( Proxy(..) )
import Database.SQLite.Simple (Connection)
import Servant.API ( Capture, type (:>), Get )
import Servant.Client ( client, ClientM )
import Servant.Server ( serve, Server, Handler, Application )

type DBScoresAPI =
  "leaderboardQuery" :> Capture "pageNum" Int :> Capture "pageSize" Int :> Get '[ScoreField] [ScoreField]

dbScoreQueryServer :: Connection -> Server DBScoresAPI
dbScoreQueryServer conn = processScores
  where
    processScores :: Int -> Int -> Handler [ScoreField]
    processScores x y = liftIO $ getScoreSlice (PageNumber x) (PageHeight y) conn

dbScoresAPI :: Proxy DBScoresAPI
dbScoresAPI = Proxy

qScoresClient :: Int -> Int -> ClientM [ScoreField]
qScoresClient = client dbScoresAPI


runApi :: Connection -> Application
runApi conn = serve dbScoresAPI (dbScoreQueryServer conn)