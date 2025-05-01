{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Logging.ServerLogger where

import Bluefin.Eff
import Bluefin.IO
import Bluefin.Stream (Stream, yield, consumeStream, yieldToList, withYieldToList)
import Control.Concurrent.STM (TChan, tryReadTChan, newTChanIO, writeTChan, newTChan)
import Control.Monad.STM (atomically)
import Data.Text (Text)
import qualified Data.Text as Text
import Network.Socket (SockAddr)
import Bluefin.Writer (Writer, tell, execWriter)
import Bluefin.Compound (useImplIn, mapHandle)
import Control.Monad.Writer (WriterT, lift)
import qualified Control.Monad.Writer.Lazy as W
import qualified System.IO as IO

data Logger i o e = Logger (Stream o e)

data ClientLogItem = ClientLog {clientID :: Int, clientAddr :: SockAddr, clientMsg :: Text}

instance Show ClientLogItem where
  show (ClientLog iD sockAddr msg) =
    "Client ID: " <> show iD <> "| Client Addr: " <> show sockAddr <> ": " <> Text.unpack msg

type ClientLogger = Logger (TChan ClientLogItem) Text

sendToLog :: (e :> es) => IOE e -> TChan ClientLogItem -> ClientLogItem  -> ClientLogger e -> Eff es ()
sendToLog io clq cl (Logger str) = do
  yield str . Text.pack $ "Writing clientID " <> show (clientID cl) <> " to the pack"
  effIO io . atomically . writeTChan clq $ cl


logClient :: (e :> es, Show i) => IOE e -> TChan i -> Logger i Text e -> Eff es ()
logClient io clq l@(Logger str) = do
  res <- effIO io . atomically $ tryReadTChan clq
  case res of
    Nothing -> pure ()
    Just cl -> do
      yield str (Text.pack . show $ cl)
      logClient io clq l


spawnLogger :: (e :> es) => Stream o e -> Eff es (Logger i o e)
spawnLogger y = pure (Logger y)


type Log2 inp = WriterT [Text] IO ()



logActionClientLoggerIO :: (e :> es) => IOE e -> IO b -> Text -> ClientLogger e -> Eff es b
logActionClientLoggerIO io ioAct logmsg (Logger str) = do
  a <- effIO io ioAct
  yield str logmsg
  pure a

logActionClientLogger :: (e :> es) => Eff es b -> Text -> ClientLogger e -> Eff es b
logActionClientLogger act logmsg (Logger str) = do
  a <- act
  yield str logmsg
  pure a

-- runLoggerBF :: IO ([Text], ())
-- runLoggerBF = runEff $ \io -> yieldToList $ \y -> do
--   logger <- spawnLogger y
--   _ <- logActionClientLoggerIO io (putStrLn "Hello") "Printing hello" logger
--   pure ()

makeLoggerW :: Monad m => WriterT [Text] m ()
makeLoggerW = pure ()

someProgram = do
  a <- logActionW (pure (2 + 2)) "Adding two numbers\n"
  putStrLn "Oh my God I'm so excited"
  putStrLn $ "The value of the equation = " <> show a


-- logActionW :: Monad m => m a -> String -> WriterT [String] m a
logActionW act logmsg = flushStdOut $ do
  a <- lift act
  W.tell logmsg
  pure a

flushW :: IO.Handle -> WriterT String IO a -> IO a
flushW h writer = do
  (result, written) <- W.runWriterT writer
  IO.hPutStr h written
  IO.hFlush h
  pure result

flushStdOut = flushW IO.stdout

