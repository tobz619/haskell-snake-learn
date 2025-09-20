{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use section" #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module DB.Server where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception (finally)
import qualified Control.Exception as E
import Control.Monad (forever, replicateM_, when)
import Control.Monad.IO.Class (MonadIO (..))
import qualified DB.Authenticate as Auth
import DB.Highscores (addScoreWithReplay, getReplayData, openDatabase)
import DB.Receive
import DB.Send (sendReplayData, SendData (..))
import DB.Servant
import Web.Scotty
import DB.Types
import Data.Binary
import Data.Coerce (coerce)
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
import Network.TLS
import System.IO (IOMode (..), hFlush, openFile)
import UI.Types
  ( SeedType,
    mkEvs,
  )
import Control.Concurrent.STM.TSem (newTSem)
import Servant.Server
import Network.Wai.Handler.Warp
import DB.Scotty (dbAPIScotty)

appPort, replayPort, leaderBoardPort :: PortNumber
appPort = 34561
replayPort = 34565
leaderBoardPort = 34566
leaderBoardPort2 = 34567

serverContext :: TCPConn -> IO TLSConn
serverContext (TCPConn c) = do
  serverPars <- Auth.serverAuth
  TLSConn
    <$> contextNew c serverPars

main :: IO ()
main = do
  appChan <- newTChanIO -- Channel for messages to come back to
  replayChan <- newTChanIO
  threadPool <- atomically $ newTSem (fromIntegral maxPlayers) -- The threadpool of available slots to use
   -- ^Making MAXPLAYERS spaces in the threadPool
  state <- newTMVarIO newServerState -- The server state being created
  dbConn <- openDatabase "highscores.db" -- The connection to the highscores DB
  appLog <- openFile "BSLog" WriteMode
  replayLog <- openFile "Replay-Request-Log" WriteMode
  putStrLn $ "Running App Server on port:" <> show appPort <> " ..."
  putStrLn $ "Running DB Server on port:" <> show replayPort <> " ..."
  putStrLn $ "Running DB Query Service on port:" <> show leaderBoardPort <> " ..."
  putStrLn $ "Running DB Query Service 2 on port:" <> show leaderBoardPort2 <> " ..."
  mapConcurrently_
    id
    [ runTLSApp "0.0.0.0" appPort threadPool appChan (leaderboardApp state dbConn appChan),
      runTLSApp "0.0.0.0" replayPort threadPool replayChan (replayApp replayChan),
      runHTTPApp leaderBoardPort (runApi dbConn),
      run leaderBoardPort2 =<< dbAPIScotty dbConn,
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

runTCPServer :: HostName -> PortNumber -> (Socket -> IO a) -> IO a
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

    open address = do
      sock <- openSocket address
      setSocketOption sock ReuseAddr 1
      setSocketOption sock UserTimeout 10_000_000
      withFdSocket sock setCloseOnExecIfNeeded
      bind sock $ addrAddress address
      listen sock 1024
      pure sock

runHTTPApp :: PortNumber -> Application -> IO ()
runHTTPApp port = run (fromIntegral port) 

runTLSApp ::
  HostName ->
  PortNumber ->
  ThreadPool ->
  TChan Text ->
  (TCPConn -> TLSConn -> IO ()) ->
  IO ()
runTLSApp host p tp msgChan actions = do
  runTCPServer host p app
  where
    app sock = do
      !s <- fst <$> accept sock
      concurrently_
        ( do
            -- textWriteTChan msgChan $ "Incoming connection from: " <> show a
            !ctx <- serverContext (coerce s)
            let !cliConn = TCPConn s
            flip E.finally (requestClose ctx) $ validateHello tp msgChan ctx cliConn $ actions cliConn ctx
        )
        (app sock)

replayApp :: TChan Text -> TCPConn -> TLSConn -> IO ()
replayApp msgChan cliConn tlsConn = do
  sockAddr <- getPeerName $ coerce cliConn
  textWriteTChan msgChan $ "Accepted replay connection from " <> show sockAddr
  dbConn <- openDatabase "highscores.db" -- The connection to the highscores DB
  sendReplayApp dbConn tlsConn
  where
    sendReplayApp db conn = do
      scoreID <- recvInfo @TLSConn conn messageToScoreID
      replayData <- getReplayData scoreID db
      mapM_
        ( \datum ->
            textWriteTChan msgChan ("Sending replay of scoreID: " <> show scoreID)
              >> sendReplayData conn datum
        )
        replayData

leaderboardApp ::
  TMVar ServerState ->
  DB.Connection ->
  TChan Text ->
  ClientConnection ->
  TLSConn ->
  IO ()
leaderboardApp state dbConn messageChan cliConn tlsConn = do
  sockAddr <- getPeerName $ coerce cliConn
  textWriteTChan messageChan $ "Accepted leaderboard connection from " <> show sockAddr
  st <- atomically $ takeTMVar state

  (!cix, !cc) <- case addClient st cliConn of -- Return the index that the player was inserted at
    Left MaxPlayers -> atomically (putTMVar state st) >> pure (-1, -1) -- figure out what to do if the queue of scores being uploaded is full; ideally create a queue and process asynchronously while not full
    Right (newSt, ix) -> do
      atomically $ putTMVar state newSt
      textWriteTChan messageChan (show newSt <> " Size: " <> show (numClients newSt))
      ix `seq` clientCount newSt `seq` pure (ix, clientCount newSt)
    Left _ -> error "Impossible"

  if cix < 0
    then leaderboardApp state dbConn messageChan cliConn tlsConn
    else flip finally (disconnectMsg cliConn >> disconnect cix state) $ do
      serverApp cix cc dbConn tlsConn cliConn messageChan
  where
    disconnectMsg conn = do
      textWriteTChan messageChan $ "Disconnecting " <> show conn

serverApp :: CIndex -> Int -> DB.Connection -> TLSConn -> ClientConnection -> TChan Text -> IO ()
serverApp cix cliCount dbConn tlsConn tcpConn messageChan = E.handle recvHandler $ do
  textWriteTChan messageChan $ replicate 90 '='
  textWriteTChan messageChan $ "Client at index: " <> show cix
  s <- recvInfo tlsConn messageToScore
  textWriteTChan messageChan $ "Score of " <> show s <> " received"
  name <- recvInfo tlsConn messageToName
  textWriteTChan messageChan $ "Name of " <> show (Text.toUpper name) <> " received"
  seedBytes <- recvInfo tlsConn id
  let seedValue = decode @SeedType seedBytes
  let seed = messageToSeed seedBytes
  textWriteTChan messageChan $ "Seed: " <> show seed
  evListBytes <- recvInfo tlsConn id
  -- textWriteTChan messageChan $ "evListBytes: " <> show evListBytes
  let evList = mkEvs $ handleEventList evListBytes
  textWriteTChan messageChan $ "evList Length: " <> show (length evList)
  -- textWriteTChan messageChan $ "All events: " <> show evList

  -- Run the game replay
  let !game = runReplayG evList (initState seed)
      s' = (score . getWorld) game
  if s /= s'
    then do
      textWriteTChan messageChan $ "Mismatched score from clientIndex " <> show cix <> "!"
      textWriteTChan messageChan $ "Expected score: " <> show s
      textWriteTChan messageChan $ "Actual score: " <> show s'
    else do
      textWriteTChan messageChan "Valid score" -- placeholder
      time <- liftIO (round <$> getPOSIXTime)
      addScoreWithReplay name s time seedValue evListBytes dbConn
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
