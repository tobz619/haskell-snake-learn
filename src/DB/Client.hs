{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module DB.Client where

-- import Data.Int (Int64)

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race, replicateConcurrently_, wait, withAsync)
import Control.Concurrent.STM (atomically, flushTQueue, newTQueueIO, writeTQueue)
import qualified Control.Exception as E
import qualified DB.Authenticate as Auth
import DB.Receive
import DB.Send
import DB.Server (httpPort)
import DB.Types
import Data.Binary (decode)
import qualified Data.ByteString as BS
import Data.Coerce (coerce)
import Data.Default
import Data.List (scanl', intercalate)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as Text
import GameLogic (ScoreType)
import qualified Network.HTTP as HTTP
import Network.Socket
import Network.TLS
import System.Random (mkStdGen)
import UI.Types
  ( EventList,
    GameEvent (..),
    KeyEvent (MoveDown, MoveLeft, MoveRight, MoveUp),
    SeedType,
    TickNumber (TickNumber),
  )
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import Data.Binary.Put
import qualified Data.Bimap as BM
import qualified Data.ByteString.Lazy as BL

serverName, serverName' :: HostName
serverName = "127.0.0.1"
serverName' = "haskell-server.tobioloke.com"

clientPort,
  replayPort,
  clientPort',
  replayPort',
  leaderBoardPort,
  leaderBoardPort' ::
    PortNumber
clientPort = httpPort
clientPort' = 5000
replayPort = 34565
replayPort' = 5050
leaderBoardPort = httpPort
leaderBoardPort' = 5051

serv :: HostName
serv = serverName

cli :: PortNumber
cli = clientPort

clientContext :: BS.ByteString -> TCPConn -> IO TLSConn
clientContext ident (TCPConn c) = do
  params <- Auth.clientAuth ident
  TLSConn
    <$> contextNew c params

clientContextLeaderboard, clientContextQuery :: TCPConn -> IO TLSConn
clientContextLeaderboard = clientContext "leaderboard"
clientContextQuery = clientContext "query"

runClientAppSTM :: SeedType -> ScoreType -> Text.Text -> EventList -> IO ()
runClientAppSTM seed score name evList = withSocketsDo $ do
  q <- newTQueueIO
  runTLSClient serv cli (app q)
  where
    app tq c = do
      actions <- atomically $ do
        writeTQueue tq $ putStrLn "Sending hello message"
        writeTQueue tq $ sendHello c
        writeTQueue tq $ putStrLn $ "Sending seed: " ++ show (mkStdGen 4)
        writeTQueue tq $ sendScoreMessage c score
        writeTQueue tq $ sendName c name
        writeTQueue tq $ sendSeedMessage c seed
        writeTQueue tq $ sendEventList c evList
        writeTQueue tq $ putStrLn "Closing conn"
        flushTQueue tq
      sequence_ actions

recvReplayData :: Int -> IO (Maybe ReplayData)
recvReplayData scoreID = withSocketsDo $ do
  runTLSClient serv replayPort app
  where
    app ctx = do
      sendHello ctx
      handleConnection ctx

    getData conn =
      Just
        <$> ( ReplayData
                <$> recvInfo conn (decode @Int)
                <*> recvInfo conn id
            )

    recvHandler HelloTooSlow = pure Nothing -- Replace with Error Dialog
    recvHandler WrongHello = pure Nothing -- Replace with Error Dialog
    recvHandler e = E.throwIO e

    handleConnection conn = E.handle recvHandler $ do
      errorOrReplay <- race (threadDelay 5_000_000) (sendScoreFieldID conn scoreID >> getData conn)
      either
        (\_ -> E.throwIO HelloTooSlow >> pure Nothing)
        pure
        errorOrReplay

runTCPClient :: HostName -> PortNumber -> (TCPConn -> IO b) -> IO b
runTCPClient hostName port action = flip withAsync wait $ do
  addr <- resolve
  E.bracketOnError (connectClientTCPSocket addr) requestClose action
  where
    resolve = do
      let hints = defaultHints {addrSocketType = Stream}
      NE.head <$> getAddrInfo (pure hints) (pure hostName) (pure (show port))

    connectClientTCPSocket addr = E.bracketOnError
      (socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr))
      close
      $ \sock -> do
        setSocketOption sock NoDelay 1
        setSocketOption sock Linger 5
        connect sock $ addrAddress addr
        pure $ TCPConn sock

runTLSClient :: HostName -> PortNumber -> (TLSConn -> IO b) -> IO b
runTLSClient hostName port acts = withSocketsDo $ do
  runTCPClient hostName port app
  where
    app tcpConn = flip E.finally (requestClose tcpConn) $ do
      ctx <- clientContextQuery tcpConn
      handshake (coerce ctx)
      print =<< getPeerName (coerce tcpConn)
      acts ctx

testClient :: IO ()
testClient =
  let moves = [1, 3, 8, 10, 1, 11, 14, 1, 1]
      events =
        zipWith
          (GameEvent . TickNumber)
          (scanl' (+) 1 moves)
          [MoveRight, MoveDown, MoveLeft, MoveUp, MoveRight, MoveDown, MoveRight, MoveDown, MoveLeft]
   in do
        runClientAppSTM 4 4 ("MALCOLM" :: NameType) events

replicateClients :: Int -> IO ()
replicateClients = flip replicateConcurrently_ testClient

manyTestClients :: Int -> IO ()
manyTestClients n = mapM_ (\(v, act) -> act >> print v) $ zip ([1 ..] :: [Int]) (replicate n testClient)

-- repTest = do
--  evs <- newMVar events
--  runReplayApp (mkStdGen 4) evs

leaderBoardRequest :: PageNumber -> PageHeight -> IO [ScoreField]
leaderBoardRequest (PageNumber pix) (PageHeight psize) =
  do
    res <- req
    rcode <- HTTP.getResponseCode res
    case rcode of
      (2,0,0) -> read @[ScoreField] <$> HTTP.getResponseBody res
      a -> print a >> error "Unable to get scores"

  where req = HTTP.simpleHTTP
                ( HTTP.getRequest
                    ( "http://localhost:34566/leaderboardQuery/"
                        ++ show pix
                        ++ "/"
                        ++ show psize
                        ++ "/"
                    )
                )
postScoreLeaderBoard :: NameType -> ScoreType -> SeedType -> EventList -> IO ()
postScoreLeaderBoard name score seed evlist = do
  res <- req
  rcode <- HTTP.getResponseCode res
  case rcode of
    (2,0,_) -> pure ()
    a -> print a >> error "Failed to upload score"

  where req = HTTP.simpleHTTP
              (
                HTTP.postRequestWithBody
                (
                  "http://localhost:34566/addScore/"
                  ++ intercalate "/" [T.unpack name, show score, show seed]
                )
                "application/text"
                ( show converted )
              )

        converted = runPut (mapM_ gameEvPutter evlist)
        gameEvPutter (GameEvent t ev) = do
                    maybe (putLazyByteString BL.empty) putWord8 (BM.lookup ev keyEvBytesMap)
                    putWord16be (fromIntegral t)