{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use section" #-}

module DB.Server where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception (finally)
import qualified Control.Exception as E
import Control.Monad (forever, replicateM_, when)
import Control.Monad.IO.Class (MonadIO (..))
import DB.Highscores (addScoreWithReplay, getReplayData, openDatabase)
import DB.Receive
import DB.Send (sendReplayData)
import DB.Types
import Data.Binary
import qualified Data.IntMap.Strict as IMap
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Database.SQLite.Simple as DB
import GameLogic (GameState (getWorld), World (..))
import Logging.Replay (initState, runReplayG)
import Network.Socket
import System.IO (IOMode (..), hFlush, openFile)
import UI.Types
  ( mkEvs,
  )

appPort, viewPort :: PortNumber
appPort = 34561
viewPort = 34565

main :: IO ()
main = do
  appChan <- newTChanIO -- Channel for messages to come back to
  replayChan <- newTChanIO
  threadPool <- newTQueueIO -- The threadpool of available slots to use
  appLog <- openFile "BSLog" WriteMode
  replayLog <- openFile "Replay-Request-Log" WriteMode
  putStrLn $ "Running AppServer on localhost:" <> show appPort <> " ..."
  putStrLn $ "Running DBserver on localhost:" <> show viewPort <> " ..."
  mapConcurrently_
    id
    [ runTCPServer "0.0.0.0" appPort (leaderboardApp appChan threadPool),
      runTCPServer "0.0.0.0" viewPort (replayApp replayChan threadPool),
      receive appChan appLog,
      receive replayChan replayLog
    ]
  where
    whileM iob act = do
      b <- iob
      when b $ act >> whileM iob act

    receive msgs logFile =
      forever $
        whileM
          (not <$> atomically (isEmptyTChan msgs))
          ( atomically (readTChan msgs) >>= \msg ->
              Text.putStrLn msg
                >> Text.hPutStrLn logFile msg
                >> hFlush logFile
                -- >> threadDelay 1_000
          )

---------------------------------------------------
-- Server stuff

runTCPServer :: String -> PortNumber -> (Socket -> IO a) -> IO a
runTCPServer host p app = withSocketsDo $ forever $ do
  addr <- resolve
  E.bracketOnError (open addr) close app
  where
    resolve = do
      let hints =
            defaultHints
              { addrFlags = [AI_PASSIVE],
                addrSocketType = Stream
              }
      NE.head <$> getAddrInfo (Just hints) (Just host) (Just (show p))

    open addr = do
      sock <- openSocket addr
      setSocketOption sock ReuseAddr 1
      setSocketOption sock UserTimeout 7_000_000
      withFdSocket sock setCloseOnExecIfNeeded
      bind sock $ addrAddress addr
      listen sock 1024
      pure sock

replayApp :: TChan Text -> ThreadPool -> Socket -> IO ()
replayApp msgChan threadPool sock = do
  dbConn <- openDatabase "highscores.db" -- The connection to the highscores DB
  awaitConnection dbConn
  
  where
    awaitConnection db = do 
      (s, a) <- accept sock  
      textWriteTChan msgChan $ "Accepted query connection from " <> show a
      concurrently_
        (validateHello threadPool msgChan (TCPConn s) (sendReplayApp db))
        (awaitConnection db)
    
    sendReplayApp db conn = do
      scoreID <- recvTCPData conn messageToScoreID
      replayData <- getReplayData db scoreID
      maybe (pure ()) (sendReplayData conn) replayData

leaderboardApp :: TChan Text -> ThreadPool -> Socket -> IO ()
leaderboardApp messageChan threadPool sock = do
  state <- newTMVarIO newServerState -- The server state being created
  dbConn <- openDatabase "highscores.db" -- The connection to the highscores DB
  atomically <$> replicateM_ maxPlayers $ writeTQueue threadPool () -- Making MAXPLAYERS spaces in the threadPool
  awaitConnection state dbConn
  where
    awaitConnection state dbConn =
      do
        (s, a) <- accept sock
        -- inp <- S.nullInput
        -- a' <- flip decodeHAProxyHeaders inp =<< socketToProxyInfo s a
        -- handshake ctx
        textWriteTChan messageChan $ "Accepted leaderboard connection from " <> show a
        -- sendAll s "Connected\n"
        concurrently_ 
          (handleAppConnection state dbConn threadPool (TCPConn s) messageChan) 
          (awaitConnection state dbConn)

handleAppConnection :: TMVar ServerState -> DB.Connection -> ThreadPool -> ClientConnection -> TChan Text -> IO ()
handleAppConnection state dbConn threadPool cliConn messageChan =
  flip E.finally (atomically $ writeTQueue threadPool ()) $
    E.handle recvHandler $
      validateHello threadPool messageChan cliConn addClientApp
  where
    recvHandler UnexpectedClose = do
      textWriteTChan messageChan $ "Unexpected closure - lost connection with: " <> show (getSocket cliConn)
    recvHandler e = E.throwIO e

    addClientApp _ = do
      st <- atomically $ takeTMVar state
      -- textWriteTChan messageChan "Taking app state"

      (!cix, !cc) <- case addClient st cliConn of -- Return the index that the player was inserted at
        Left MaxPlayers -> atomically (putTMVar state st) >> pure (-1, -1) -- figure out what to do if the queue of scores being uploaded is full; ideally create a queue and process asynchronously while not full
        Right (newSt, ix) -> do
          atomically $ putTMVar state newSt
          textWriteTChan messageChan (show newSt <> " Size: " <> show (numClients newSt))
          ix `seq` clientCount newSt `seq` pure (ix, clientCount newSt)
        Left _ -> error "Impossible"

      if cix < 0
        then handleAppConnection state dbConn threadPool cliConn messageChan
        else flip finally (disconnectMsg cliConn >> disconnect cix state) $ do
          serverApp cix cc dbConn cliConn messageChan
      where
        disconnectMsg conn = do
          textWriteTChan messageChan $ "Disconnecting " <> show conn

serverApp :: CIndex -> Int -> DB.Connection -> ClientConnection -> TChan Text -> IO ()
serverApp cix cliCount dbConn tcpConn messageChan = E.handle recvHandler $ do
  textWriteTChan messageChan $ replicate 90 '='
  textWriteTChan messageChan $ "Client at index: " <> show cix
  s <- recvTCPData tcpConn messageToScore
  textWriteTChan messageChan $ "Score of " <> show s <> " received"
  name <- recvTCPData tcpConn messageToName
  textWriteTChan messageChan $ "Name of " <> show (Text.toUpper name) <> " received"
  seedBytes <- recvTCPData tcpConn id
  let seedValue = decode seedBytes
  let seed = messageToSeed seedBytes
  textWriteTChan messageChan $ "Seed: " <> show seed
  evListBytes <- recvTCPData tcpConn id
  let evList = mkEvs $ handleEventList evListBytes
  textWriteTChan messageChan $ "evList Length: " <> show (length evList)
  -- textWriteTChan messageChan $ "All events: " <> show evList

  -- Run the game replay
  let !game = runReplayG evList (initState seed)
      s' = (score . getWorld) game
  if s /= s'
    then do
      textWriteTChan messageChan $ "Mismatched score from clientIndex" <> show cix <> "!"
      textWriteTChan messageChan $ "Expected score: " <> show s
      textWriteTChan messageChan $ "Actual score: " <> show s'
    else do
      textWriteTChan messageChan "Valid score" -- placeholder
      time <- liftIO (round <$> getPOSIXTime)
      addScoreWithReplay dbConn name s time seedValue evListBytes
      textWriteTChan messageChan $ "Score of " <> show s <> " by user " <> show cliCount <> " added"
  textWriteTChan messageChan $ replicate 90 '='
  where
    recvHandler UnexpectedClose = do
      textWriteTChan messageChan $ "Lost connection with: " <> show (getSocket tcpConn)
    recvHandler e = E.throwIO e

-- Constants and ServerState functions

maxPlayers :: Int
maxPlayers = 16

newServerState :: ServerState
newServerState = ServerState 0 mempty 0

numClients :: ServerState -> Int
numClients = IMap.size . clients

-- | Adds a client to the existing @ServerState@ at the current index. If the index is full, try the
-- (next one @\`mod\`@ @maxPlayers@) recursively until a space is found. If there are more than maxPlayers, return an error.
addClient :: ServerState -> TCPConn -> Either ServerStateError (ServerState, CIndex)
addClient s@(ServerState cc cs ix) c
  | numClients s >= maxPlayers = Left MaxPlayers
  | otherwise =
      maybe
        (pure (ServerState (cc + 1) (IMap.insert ix c cs) ((cc + 1) `mod` maxPlayers), ix)) -- If no player found return a new state and the index the current player was inserted at
        (const $ addClient (s {currentIx = (ix + 1) `mod` maxPlayers}) c) -- If a player still exists at our current index, try again at index plus one
        (IMap.lookup ix (clients s)) -- Find the existing index

removeClient :: CIndex -> ClientMap -> ClientMap
removeClient = IMap.delete

disconnect :: CIndex -> TMVar ServerState -> IO ()
disconnect cix state = atomically $ do
  st <- takeTMVar state
  putTMVar state $ st {clients = removeClient cix (clients st)}
