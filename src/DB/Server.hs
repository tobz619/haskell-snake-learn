{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module DB.Server where

import Control.Concurrent (MVar, modifyMVar_, newMVar, readMVar, swapMVar, takeMVar, threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.STM (newTQueueIO, readTQueue, writeTQueue, newTChan, newTChanIO, TChan, readTVar, readTVarIO, writeTVar, newTVarIO)
import Control.Exception (Exception, finally, mask)
import qualified Control.Exception as E
import Control.Monad (forever, replicateM_, void, when)
import Control.Monad.STM (atomically)
import Control.Monad.Trans (liftIO)
import DB.Client
import DB.Highscores (Name, Score, addScore, openDatabase)
import qualified Data.Bimap as BM
import Data.Binary
import qualified Data.ByteString.Lazy as B
import qualified Data.IntMap.Strict as Map
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Database.SQLite.Simple as DB
import GameLogic (GameState (Playing, getWorld), World (..), defaultHeight, defaultWidth, initWorld)
import Logging.Logger (EventList, GameEvent (..), TickNumber (..))
import Logging.Replay (ReplayState (ReplayState), Seed, runReplayG)
import Network.Socket
import Network.Socket.ByteString.Lazy
import System.Random (mkStdGen)
import Control.Concurrent.STM.TQueue (TQueue)
import Control.Concurrent.MVar
import Control.Concurrent.STM.TVar (TVar)

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
  state <- newTVarIO newServerState
  dbConn <- openDatabase "highscores.db"
  runTCPServer "0.0.0.0" port $ application state dbConn

-- WS.runServer "127.0.0.1" port $ application state dbConn -- legacy
---------------------------------------------------
-- Server stuff

runTCPServer :: String -> PortNumber -> (Socket -> IO a) -> IO a
runTCPServer host p app = withSocketsDo $ forever $ do
  addr <- resolve
  -- print addr
  E.bracket (open addr) close app
  where
    resolve = do
      let hints =
            defaultHints
              { addrFlags = [AI_PASSIVE],
                addrSocketType = Stream
              }
      NE.head <$> getAddrInfo (Just hints) (Just host) (Just (show p))

    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
      setSocketOption sock ReuseAddr 1
      withFdSocket sock setCloseOnExecIfNeeded
      bind sock $ addrAddress addr
      listen sock maxPlayers
      pure sock

application :: TVar ServerState -> DB.Connection -> Socket ->  IO ()
application state dbconn sock = 
  mask $ \restore -> do
    threadPool <- newTQueueIO
    replicateM_ 4 . atomically $ writeTQueue threadPool ()
    (s, a) <- accept sock
    _ <- atomically $ readTQueue threadPool
    putStrLn $ "Accepted connection from " ++ show a
    flip withAsync wait $ do
    -- sendAll s "Connected\n"
      threadDelay 1000000
      restore (appHandling state dbconn threadPool (TCPConn s))

appHandling :: TVar ServerState -> DB.Connection -> TQueue () -> ClientConnection -> IO ()
appHandling state dbConn threadpool cliConn = do
  st <- readTVarIO state
  putStrLn $ "Index is " ++ show (currentIx st)
  cix <- case addClient st cliConn of
    Left MaxPlayers -> pure (-1) -- figure out what to do if the queue of scores being uploaded is full; ideally create a queue and process asynchronously while not full
    Right newSt -> do
      atomically $ writeTVar state newSt
      print newSt
      pure (currentIx newSt)
    Left _ -> error "Impossible"

  atomically $ writeTQueue threadpool ()
  flip finally (disconnect cix state) $
    serverApp cix dbConn cliConn

serverApp :: CIndex -> DB.Connection -> ClientConnection -> IO ()
serverApp cix dbConn tcpConn = do
  putStrLn $ replicate 90 '='
  putStrLn $ "Client at index: " <> show cix
  s <- recvTCPData tcpConn messageToScore
  putStrLn $ "Score of " <> show s <> " received"
  name <- recvTCPData tcpConn messageToName
  putStrLn $ "Name of " <> show (T.toUpper name) <> " received"
  seed <- recvTCPData tcpConn messageToSeed
  putStrLn $ "Seed: " <> show seed
  evList <- recvTCPData tcpConn handleEventList
  -- putStrLn $ "First three events: " <> show (take 3 evList)
  putStrLn $ "All events: " <> show evList

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
      putStrLn "Valid score" -- placeholder
      -- time <- liftIO (round <$> getPOSIXTime)
      -- addScore dbConn name s time
      -- putStrLn $ "Score of " <> show s <> " by user " <> show cix <> " added"
  putStrLn $ replicate 90 '='
  gracefulClose (getSocket tcpConn) 500

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
        (Right $ ServerState (Map.insert ix c cs) (ix `mod` maxPlayers)) -- If successful return a new state and the index the current player was inserted at
        (const $ addClient s {currentIx = (ix + 1) `mod` maxPlayers} c) -- If a player still exists at our current index, try again at plus one
        (Map.lookup ix (clients s)) -- Find the existing index

removeClient :: CIndex -> ClientMap -> ClientMap
removeClient = Map.delete

disconnect :: CIndex -> TVar ServerState -> IO ()
disconnect cix state = do
  s <- readTVarIO state
  atomically $ writeTVar state $ s {clients = removeClient cix (clients s)}

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
