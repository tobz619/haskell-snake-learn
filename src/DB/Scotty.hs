{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
module DB.Scotty where

import Web.Scotty
import DB.Highscores (getScoreSlice, addScoreWithReplay)
import Control.Monad.IO.Class (liftIO)
import DB.Types (PageHeight(PageHeight), PageNumber (PageNumber), NameType, EventListMessage, Time)
import Network.HTTP.Types.Status
import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Database.SQLite.Simple as DB
import Servant.Server (Application)
import qualified Data.Text.Lazy as T
import GameLogic (ScoreType, getWorld, score)
import UI.Types (SeedType, EventList, mkEvs)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Logging.Replay (runReplayG, initState, Seed)
import DB.Receive (handleEventList, textWriteTChan)
import System.Random (mkStdGen)
import Control.Concurrent.STM (TChan)
import Data.Text (Text)
import Data.List (find)

scottyAPI :: TChan Text -> DB.Connection -> ScottyM ()
scottyAPI msgChan conn = do
  get "/leaderboardQuery/:pageIndex/:pageSize" $ getScoreSlice' msgChan conn
  post "/addScore/:name/:score/:seedValue" $ addScoreToDB' msgChan conn

getScoreSlice' :: TChan Text -> DB.Connection -> ActionM ()
getScoreSlice' msgChan dbConn = do
  ip <- getIPFromHeaders
  liftIO $ textWriteTChan msgChan $ "Remote IP: " ++ ip
  pIx <- PageNumber <$> pathParam "pageIndex"
  pSize <- PageHeight <$> pathParam "pageSize"
  ps <- liftIO $ getScoreSlice pIx pSize dbConn
  case ps of
    [] -> do
      status status400
      text "No values returned"
    ps' -> do
      status status200
      text $ T.pack . show $ ps'

  where getIPFromHeaders = do
            hds <- headers
            let res = maybe "N/A" snd $ find ((== "HTTP_CLIENT_IP") . fst ) hds
            pure (T.unpack res) 

addScoreToDB' :: TChan Text -> DB.Connection -> ActionM ()
addScoreToDB' msgChan dbConn = do
  name <- pathParam  @NameType "name"
  s <- pathParam @ScoreType "score"
  seed <- pathParam @SeedType "seedValue"
  evListStr <- B8.unpack . B8.take 8192 <$> body
  liftIO $ putStrLn evListStr
  let evList = mkEvs $ handleEventList evListBytes
      evListBytes = read @EventListMessage evListStr
      !game = runReplayG evList (initState (mkStdGen seed))
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
      liftIO $ addScoreWithReplay name s time seed evListBytes dbConn
      liftIO $ textWriteTChan msgChan $ "Score of " <> show s <> " added"
      status status201
      text "OK"


dbAPIScotty :: TChan Text -> DB.Connection -> IO Application
dbAPIScotty msgChan = scottyApp . scottyAPI msgChan