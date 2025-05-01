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
import Control.Concurrent.STM (TChan, tryReadTChan, newTChanIO, writeTChan, newTChan, readTChan)
import Control.Monad.STM (atomically)
import Data.Text (Text)
import qualified Data.Text as Text
import Network.Socket (SockAddr)
import Bluefin.Writer (Writer, tell, execWriter, runWriter)
import Bluefin.Compound (useImplIn, mapHandle)
import Control.Monad.Writer (WriterT, lift)
import qualified Control.Monad.Writer.Lazy as W
import qualified System.IO as IO

newtype Logger i o e = Logger (Writer o e)
type ClientLogger = Logger (TChan ClientLogItem) Text

data ClientLogItem = ClientLog {clientID :: Int, clientAddr :: SockAddr, clientMsg :: Text}



instance Show ClientLogItem where
  show (ClientLog iD sockAddr msg) =
    "Client ID: " <> show iD <> "| Client Addr: " <> show sockAddr <> ": " <> Text.unpack msg


sendToQueue :: (e1 :> es, e :> es) => IOE e1 -> TChan ClientLogItem -> ClientLogItem -> ClientLogger e -> Eff es ()
sendToQueue io clq cl = 
  logActionClientLoggerIO io 
  (atomically $ writeTChan clq cl) 
  ("Writing clientID " <> Text.pack (show (clientID cl)) <> " to the queue")
  


-- logClient :: (e :> es) => IOE e -> TChan ClientLogItem -> ClientLogger e -> Eff es ()
-- logClient :: (e1 :> es, e :> es) => IOE e1 -> TChan ClientLogItem -> ClientLogger e -> Eff es ClientLogItem
logClient :: (e1 :> es, e :> es) => IOE e1 -> TChan ClientLogItem -> ClientLogger e -> Eff es ClientLogItem
logClient io clq l = do 
  cl <- logActionClientLoggerIO io (atomically $ readTChan clq) "Pulling from the client from the queue" l
  logActionClientLoggerIO io (pure cl) ("Logged client: " <> Text.pack (show cl)) l



-- spawnClientLogger :: (e :> es) => (forall e. ClientLogger e -> Eff (e :& es) r) -> Eff es r
spawnClientLogger :: (e :> es) => Writer Text e -> (forall e. ClientLogger e -> Eff (e :& es) r) -> Eff es r
spawnClientLogger w k = useImplIn k $ Logger (mapHandle w)


logActionClientLoggerIO :: (e1 :> es, e :> es) => IOE e1 -> IO b -> Text -> ClientLogger e -> Eff es b
logActionClientLoggerIO io ioAct logmsg (Logger str) = do
  a <- effIO io ioAct
  tell str logmsg
  pure a

logActionClientLogger :: (e :> es) => Eff es b -> Text -> ClientLogger e -> Eff es b
logActionClientLogger act logmsg (Logger str) = do
  a <- act
  tell str logmsg
  pure a

-- runLoggerBF :: IO ([Text], ())
-- runLoggerBF :: (forall e. ClientLogger e -> Eff (e :& es) r) -> IO ([Text], r)
someProgramBF io = do
  chan <- effIO io newTChanIO
  execWriter $ \w -> spawnClientLogger w $ do
    sendToQueue io chan (ClientLog undefined undefined undefined)
    -- sendToQueue io chan (ClientLog undefined undefined undefined)

  -- clientLogger $ sendToQueue io chan (ClientLog undefined undefined undefined)
  -- clientLogger $ logActionClientLoggerIO io (putStrLn "Wow") "What is this?"
  -- clientLogger $ logClient io chan
  
  pure ()
    -- logActionClientLoggerIO io (putStrLn "Wow") "What is this?"





makeLoggerW :: Monad m => WriterT [Text] m ()
makeLoggerW = pure ()

someProgram :: IO ()
-- >>> someProgram
someProgram = do
  a <- do logActionW (pure ()) "Not adding two numbers, just messing about"
          logActionW (pure (12+19)) "Can I add another two?"
  putStrLn "Oh my God I'm so excited"
  putStrLn $ "The value of the equation = " <> show a


-- logActionW :: Monad m => m a -> String -> WriterT [String] m a
logActionW :: IO a -> String -> IO a
logActionW act logmsg = flushStdOut $ do
  a <- lift act
  W.tell . unlines . pure $ logmsg
  pure a

flushW :: IO.Handle -> WriterT String IO a -> IO a
flushW h writer = do
  (result, written) <- W.runWriterT writer
  IO.hPutStr h written
  IO.hFlush h
  pure result

flushStdOut :: WriterT String IO a -> IO a
flushStdOut = flushW IO.stdout

