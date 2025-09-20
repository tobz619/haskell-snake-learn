{-# LANGUAGE OverloadedStrings #-}
module DB.Scotty where

import Web.Scotty
import DB.Highscores (getScoreSlice)
import Control.Monad.IO.Class (liftIO)
import DB.Types (PageHeight(PageHeight), PageNumber (PageNumber))
import Network.HTTP.Types.Status
import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Database.SQLite.Simple as DB
import Servant.Server (Application)

scottyAPI :: DB.Connection -> ScottyM ()
scottyAPI conn = do
  get "/leaderBoardQuery2/:pageIndex/:pageSize" $ getScoreSlice' conn

getScoreSlice' :: DB.Connection -> ActionM ()
getScoreSlice' dbConn = do
  pIx <- PageNumber <$> pathParam "pageIndex"
  pSize <- PageHeight <$> pathParam "pageSize"
  ps <- liftIO $ getScoreSlice pIx pSize dbConn
  case ps of
    [] -> do
      status status400
      text "No values returned"
    ps -> do
      status status200
      raw $ B8.pack . show $ ps


dbAPIScotty :: DB.Connection -> IO Application
dbAPIScotty = scottyApp . scottyAPI