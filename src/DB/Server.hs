{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use section" #-}

module DB.Server where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM (TChan, TQueue, isEmptyTChan, newTChanIO, newTQueueIO, readTChan, readTQueue, retry, writeTChan, writeTQueue)
import Control.Exception (Exception, finally)
import qualified Control.Exception as E
import Control.Monad (forever, replicateM_, when)
import Control.Monad.STM (atomically)
import DB.Client
import DB.Highscores (Name, Score, openDatabase)
import qualified Data.Bimap as BM
import Data.Binary
import qualified Data.ByteString.Lazy as B
import qualified Data.IntMap.Strict as Map
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Database.SQLite.Simple as DB
import GameLogic (GameState (Playing, getWorld), World (..), defaultHeight, defaultWidth, initWorld)
import Logging.Logger (EventList, GameEvent (..), TickNumber (..))
import Logging.Replay (ReplayState (ReplayState), Seed, runReplayG)
import Network.Socket
import Network.Socket.ByteString.Lazy
import System.IO (IOMode (..), hClose, hFlush, hPutStrLn, openFile)
import System.Random (mkStdGen)

data ServerState = ServerState {clients :: ClientMap, currentIx :: CIndex}
  deriving (Show)

type ClientMap = Map.IntMap TCPConn

type CIndex = Int

type ClientConnection = TCPConn

data ServerStateError = MaxPlayers | UnexpectedClose deriving stock (Show, Eq)

instance Exception ServerStateError

port :: PortNumber
port = 34561

main :: IO ()
main = do
  putStrLn $ "Running server on localhost:" <> show port <> " ..."
  runTCPServer "0.0.0.0" port $ application

---------------------------------------------------
-- Server stuff

runTCPServer :: String -> PortNumber -> (Socket -> IO a) -> IO a
runTCPServer host p app = withSocketsDo $ forever $ do
  addr <- resolve
  E.bracketOnError (open addr) (close) app
  where
    resolve = do
      let hints =
            defaultHints
              { addrFlags = [AI_PASSIVE],
                addrSocketType = Stream
              }
      NE.head <$> getAddrInfo (Just hints) (Just host) (Just (show p))

    open addr = E.bracketOnError (openSocket addr) (close) $ \sock -> do
      setSocketOption sock ReuseAddr 1
      setSocketOption sock UserTimeout 15_000_000
      -- withFdSocket sock setCloseOnExecIfNeeded
      bind sock $ addrAddress addr
      listen sock 128
      pure sock

application :: Socket -> IO ()
application sock = do
  messageChan <- newTChanIO -- Channel for messages to come back to
  threadPool <- newTQueueIO -- The threadpool of available slots to use
  state <- newMVar newServerState -- The server state being created
  dbConn <- openDatabase "highscores.db" -- The connection to the highscores DB
  replicateM_ maxPlayers . atomically $ writeTQueue threadPool () -- Making MAXPLAYERS spaces in the threadPool
  lf <- openFile "BSLog" WriteMode

  concurrently_ (receive messageChan lf) (awaitConnection state threadPool dbConn messageChan)

  hClose lf
  where
    whileM iob act = do
      b <- iob
      when b $ act >> whileM iob act
    receive msgs logFile =
      forever $
        whileM
          (not <$> atomically (isEmptyTChan msgs))
          ( atomically (readTChan msgs)
              >>= \msg ->
                Text.putStrLn msg
                  >> Text.hPutStrLn logFile msg
                  >> hFlush logFile
                  >> threadDelay 1_000
          )

    awaitConnection state threadPool dbConn messageChan =
      do
        _ <- atomically $ readTQueue threadPool
        (s, a) <- accept sock
        -- putStrLn $ "Accepted connection from " ++ show a
        textWriteTChan messageChan $ "Accepted connection from " ++ show a
        -- sendAll s "Connected\n"
        concurrently_
          (handleAppConnection state dbConn threadPool (TCPConn s) messageChan)
          (awaitConnection state threadPool dbConn messageChan)

handleAppConnection :: MVar ServerState -> DB.Connection -> TQueue () -> ClientConnection -> TChan Text -> IO ()
handleAppConnection state dbConn threadpool cliConn messageChan = do
  st <- takeMVar state
  let indexName = Text.pack $ "Index is " ++ show (currentIx st)
  Text.putStrLn indexName
  -- runEff_ $ \io -> logAction logger indexName
  cix <- case addClient st cliConn of -- Return the index that the player was inserted at
    Left MaxPlayers -> atomically retry -- figure out what to do if the queue of scores being uploaded is full; ideally create a queue and process asynchronously while not full
    Right newSt -> do
      putMVar state newSt
      textWriteTChan messageChan (show newSt)
      pure (currentIx st)
    Left _ -> error "Impossible"

  flip finally (disconnect cix state >> atomically (writeTQueue threadpool ())) $ do
    serverApp cix dbConn cliConn messageChan

serverApp :: CIndex -> DB.Connection -> ClientConnection -> TChan Text -> IO ()
serverApp cix dbConn tcpConn messageChan = do

  textWriteTChan messageChan $ replicate 90 '='
  textWriteTChan messageChan $ "Client at index: " <> show cix
  s <- recvTCPData tcpConn messageToScore
  textWriteTChan messageChan $
      "Score of " <> show s <> " received"
  name <- recvTCPData tcpConn messageToName
  -- putStrLn $ "Name of " <> show (T.toUpper name) <> " received"
  textWriteTChan messageChan $ "Name of " <> show (T.toUpper name) <> " received"
  seed <- recvTCPData tcpConn messageToSeed
  -- putStrLn $ "Seed: " <> show seed
  textWriteTChan messageChan $ "Seed: " <> show seed
  evList <- recvTCPData tcpConn handleEventList
  -- putStrLn $ "First three events: " <> show (take 3 evList)
  -- putStrLn $ "All events: " <> show evList
  textWriteTChan messageChan $ "All events: " <> show evList

  -- Run the game replay
  let initState =
        ReplayState
          (Playing $ initWorld defaultHeight defaultWidth seed)
          (TickNumber 0)
      !game = runReplayG evList initState
      s' = (score . getWorld) game
  if s /= s'
    then do
      putStrLn "Mismatched score!"
      putStrLn $ "Expected score: " <> show s
      putStrLn $ "Actual score: " <> show s'
    else do
      textWriteTChan messageChan "Valid score" -- placeholder
      -- time <- liftIO (round <$> getPOSIXTime)
      -- addScore dbConn name s time
      textWriteTChan messageChan $ "Score of " <> show s <> " by user " <> show cix <> " added"
  textWriteTChan messageChan $ replicate 90 '='
  -- threadDelay 50_000

  -- gracefulClose (getSocket tcpConn) 500

textWriteTChan :: TChan Text -> String -> IO ()
textWriteTChan c = atomically . writeTChan c. Text.pack

--- Receiving Functions

recvTCPData :: TCPConn -> (BSMessage b -> b) -> IO b
recvTCPData tcpConn handler = do
  lenPrefixBytes <- recvAll tcpConn (fromIntegral lenBytes)
  let msglen = (fromIntegral . decode @MsgLenRep) lenPrefixBytes
  handler <$> recvAll tcpConn msglen

recvAll :: TCPConn -> MsgLenRep -> IO B.ByteString
recvAll tcpConn size
  | size <= 0 = pure B.empty
  | otherwise = do
      bs <- recv (getSocket tcpConn) (fromIntegral size)
      -- putStrLn $ "Message size in bytes: " <> show size
      -- print bs
      when (B.null bs) $ E.throwIO UnexpectedClose
      let len = B.length bs
      if len < fromIntegral size
        then do
          let missing = size - fromIntegral len
          rest <- recvAll tcpConn missing
          pure $ bs <> rest
        else pure bs

-------
-- Constants and ServerState functions

maxPlayers :: Int
maxPlayers = 4

newServerState :: ServerState
newServerState = ServerState mempty 0

numClients :: ServerState -> Int
numClients = Map.size . clients

-- | Adds a client to the existing @ServerState@ at the current index. If the index is full, try the
-- (next one @\`mod\`@ @maxPlayers@) recursively until a space is found. If there are more than maxPlayers, return an error.
addClient :: ServerState -> TCPConn -> Either ServerStateError ServerState
addClient s@(ServerState cs ix) c
  | numClients s >= maxPlayers = Left MaxPlayers
  | otherwise =
      maybe
        (pure $ ServerState (Map.insert ix c cs) ((ix + 1) `mod` maxPlayers)) -- If successful return a new state and the index the current player was inserted at
        (const $ addClient s {currentIx = (ix + 1) `mod` maxPlayers} c) -- If a player still exists at our current index, try again at plus one
        (Map.lookup ix (clients s)) -- Find the existing index

removeClient :: CIndex -> ClientMap -> ClientMap
removeClient = Map.delete

disconnect :: CIndex -> MVar ServerState -> IO ()
disconnect cix state = do
  st <- takeMVar state
  putMVar state $ st {clients = removeClient cix (clients st)}

handleEventList :: EventListMessage -> EventList
handleEventList bs =
  let chunks = splitEvery 3 bs
      splitEvery n inp
        | B.null inp = []
        | otherwise = B.take n inp : splitEvery n (B.drop 3 inp)
   in map messageToGameEvent chunks
  where
    messageToGameEvent bss =
      let (ev, tick) = B.splitAt 1 bss
          convertedEv = (BM.!>) keyEvBytesMap ev
          t = fromIntegral $ B.index tick 0 * 255 + B.index tick 1
       in GameEvent (TickNumber t) convertedEv

messageToSeed :: SeedMessage -> Seed
messageToSeed = mkStdGen . decode

messageToScore :: ScoreMessage -> Score
messageToScore = decode

messageToName :: NameMessage -> Name
messageToName = TL.toStrict . TL.decodeUtf8
