{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Logging.ServerLogger where

import Bluefin.Compound (mapHandle, useImplIn)
import Bluefin.Eff
import Bluefin.IO
import Control.Concurrent.STM (TChan, newTChanIO, readTChan, writeTChan)
import Control.Monad.STM (atomically)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Network.Socket (SockAddr (..))
import Text.Read (readMaybe)
import Bluefin.Writer (Writer, tell, execWriter)

-- newtype Logger i o e = Logger (Writer o e)

data Logger e = Logger (IOE e) (Writer Text e)

-- type ClientLogger = Logger (TChan ClientLogItem) Text
type ClientLogger = Logger

data ClientLogItem = ClientLog {clientID :: Int, clientAddr :: SockAddr, clientMsg :: Text}

instance Show ClientLogItem where
  show (ClientLog iD sockAddr msg) =
    "[Client ID: " <> show iD <> " | Client Addr: " <> show sockAddr <> " | Msg: " <> Text.unpack msg <> "]"



spawnClientLogger :: (e1 :> es, e2 :> es) => IOE e1 -> Writer Text e2 ->  (forall e. Logger e -> Eff (e :& es) r) -> Eff es r
spawnClientLogger io w k = useImplIn k $ Logger (mapHandle io) (mapHandle w)

logAction :: (e :> es) => ClientLogger e -> Text -> Eff es ()
logAction (Logger io w) msg = do
  effIO io $ Text.putStrLn msg
  tell w (msg `Text.append` "\n")

-- sendToQueue :: TChan ClientLogItem -> ClientLogItem -> ClientLogger e -> IO ()
sendToQueue :: (e :> es) => TChan ClientLogItem -> ClientLogItem -> Logger e -> Eff es ()
sendToQueue clq cl l@(Logger io w) = do
  logAction
    l
    ("Writing clientID " <> Text.pack (show (clientID cl)) <> " to the queue")

  effIO io . atomically $ writeTChan clq cl

-- logAction clientlogger (pure ()) (Text.pack $ show cl)

-- logClient :: TChan ClientLogItem -> ClientLogger e -> IO ClientLogItem
logClient :: (e :> es, Show b) => TChan b -> Logger e -> Eff es b
logClient clq l@(Logger io _) = do
  logAction l "Pulling the client from the queue"
  cl <- effIO io $ atomically $ readTChan clq
  logAction l ("Got client: " <> Text.pack (show cl))
  pure cl


someProgramBF :: (e :> es) => IOE e -> Eff es ()
someProgramBF io = do
  chan <- effIO io newTChanIO
  a <- effIO io $ putStr "Give an number: " >> getLine
  b <- effIO io $ putStr "Give an number: " >> getLine
  out <- execWriter $ \w -> spawnClientLogger io w $ \clientLogger -> do
    logAction clientLogger "abc"
    let res = maybe "Failure" show ((+) <$> readMaybe a <*> readMaybe b)
    logAction clientLogger ("The result of the computation is: " <> Text.pack res)
    sendToQueue chan (ClientLog 1 (SockAddrInet 8080 0x0100007f) "I'm a client") clientLogger
    logAction clientLogger "What is this?"
    clientA <- logClient chan clientLogger
    sendToQueue chan clientA clientLogger

  -- effIO io $ print (length chan)
  -- effIO io $ Text.putStrLn accumLog
  effIO io $ Text.putStrLn out
  pure ()

runProgram :: IO ()
runProgram = runEff $ \io -> someProgramBF io