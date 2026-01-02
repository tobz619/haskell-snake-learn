{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module DB.Scotty where

import Control.Concurrent.STM (TChan)
import DB.Highscores (addScoreWithReplay, getScoreSlice, promptAddHighScore)
import DB.Receive (handleEventList, textWriteTChan)
import DB.Types (NameType, PageHeight (PageHeight), PageNumber (PageNumber))
import qualified Data.ByteString.Lazy.Char8 as B8
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text.Lazy as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Database.SQLite.Simple as DB
import GameLogic (ScoreType, getWorld, score)
import Logging.Replay (initState, runReplayG)
import Network.HTTP.Types.Status
    ( status200, status201, status400, status406 )
import Servant(Application)
import System.Random (mkStdGen)
import UI.Types (SeedType, mkEvs)
import Web.Scotty
    ( liftIO,
      ActionM,
      ScottyM,
      body,
      get,
      headers,
      pathParam,
      post,
      scottyApp,
      status,
      text )
import Data.Coerce (coerce)

scottyAPI :: TChan Text -> DB.Connection -> ScottyM ()
scottyAPI msgChan conn = do
  get "/leaderboardQuery/:pageIndex/:pageSize" $ getScoreSlice' msgChan conn
  get "/lowestScoreQuery/:score" $ getLowestScore' conn
  get "/heartbeat/" $ heartbeat
  post "/addScore/:name/:score/:seedValue" $ addScoreToDB' msgChan conn

heartbeat :: ActionM ()
heartbeat = status status200

getLowestScore' :: DB.Connection ->   ActionM ()
getLowestScore' dbConn = do
  s <- pathParam @ScoreType "score"
  b <- liftIO $ promptAddHighScore s dbConn
  status status200
  text . T.pack $ if b then "1" else "0"


getScoreSlice' :: TChan Text -> DB.Connection -> ActionM ()
getScoreSlice' msgChan dbConn = do
  ip <- getIPFromHeaders
  liftIO $ textWriteTChan msgChan $ "Remote IP: " ++ ip
  pIx <- PageNumber <$> pathParam "pageIndex"
  pSize <- PageHeight <$> pathParam "pageSize"
  ps <- liftIO $ getScoreSlice (5 * coerce pSize) pIx pSize dbConn
  case ps of
    [] -> do
      status status400
      text "No values returned"
    ps' -> do
      status status200
      text $ T.pack . show $ ps'
  where
    getIPFromHeaders = do
      hds <- headers
      let res = maybe "N/A" snd $ find ((== "X-Forwarded-For") . fst) hds
      pure (T.unpack res)

addScoreToDB' :: TChan Text -> DB.Connection -> ActionM ()
addScoreToDB' msgChan dbConn = do
  name <- pathParam @NameType "name"
  s <- pathParam @ScoreType "score"
  seed <- pathParam @SeedType "seedValue"
  evList <- B8.take (4096 * 1024) <$> body
  let evEvents = mkEvs $ handleEventList evList
      !game = runReplayG evEvents (initState (mkStdGen seed))
      s' = (score . getWorld) game

  if s /= s'
    then do
      liftIO $ textWriteTChan msgChan $ "Mismatched score!"
      liftIO $ textWriteTChan msgChan $ "Expected score: " <> show s
      liftIO $ textWriteTChan msgChan $ "Actual score: " <> show s'
      status status406
      text "ERROR"
    else do
      liftIO $ textWriteTChan msgChan $ "Valid score"
      time <- liftIO (round <$> getPOSIXTime)
      liftIO $ addScoreWithReplay name s time seed evList dbConn
      liftIO $ textWriteTChan msgChan $ "Score of " <> show s <> " added"
      status status201
      text "OK"

dbAPIScotty :: TChan Text -> DB.Connection -> IO Application
dbAPIScotty msgChan = scottyApp . scottyAPI msgChan