{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module DB.Client where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race, replicateConcurrently_, wait, withAsync)
import Control.Concurrent.STM (atomically, flushTQueue, newTQueueIO, writeTQueue)
import qualified Control.Exception as E
import qualified DB.Authenticate as Auth
import DB.Receive ( RecvData(recvInfo) )
import DB.Send
    ( sendEventList,
      sendHello,
      sendName,
      sendScoreFieldID,
      sendScoreMessage,
      sendSeedMessage,
      SendData(requestClose) )
import DB.Server (httpPort)
import DB.Types
import qualified Data.Bimap as BM
import Data.Binary (decode)
import Data.Binary.Put
    ( putWord8, putLazyByteString, putWord16be, runPut )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Coerce (coerce)
import Data.List (intercalate, scanl')
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text as Text
import GameLogic (ScoreType)
import Lens.Micro ( (^.), (&), (.~) )
import Network.Socket
import Network.TLS ( contextNew, handshake )
import qualified Network.Wreq as Wreq
import System.Random (mkStdGen)
import Text.Read (readMaybe)
import UI.Types
  ( EventList,
    GameEvent (..),
    KeyEvent (MoveDown, MoveLeft, MoveRight, MoveUp),
    SeedType,
    TickNumber (TickNumber),
  )
import qualified Network.Wreq.Session as WreqS

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
replayPort' = 5052
leaderBoardPort = httpPort
leaderBoardPort' = 5050

serv :: HostName
serv = serverName'

cli :: PortNumber
cli = clientPort'

opts :: Wreq.Options
opts = Wreq.defaults 
       & Wreq.checkResponse .~ (Just $ \_ _-> pure ()) -- Prevent exceptions getting raised on failed connection


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
  runTLSClient serv replayPort' app
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
        (\_ -> E.throwIO ConnectTimeout)
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

heartbeatRequest :: WreqS.Session -> IO Bool
heartbeatRequest sess = wrapRequest False $ do
  res <- WreqS.getWith opts sess (intercalate "/" ["https://" ++ serverName' ++ ":" ++ show leaderBoardPort', "heartbeat"])
  pure $ (== 200) (res ^. Wreq.responseStatus . Wreq.statusCode)

-- | Wrap a request to provide a default action if an exception is raised
wrapRequest :: a -> IO a -> IO a
wrapRequest def = E.handle requestHandler
  where requestHandler ConnectFailure = pure def
        requestHandler _ = pure def

highScoreRequest :: ScoreType -> WreqS.Session -> IO Bool
highScoreRequest s sess = wrapRequest False $ do
  res <- req
  let rcode = res ^. Wreq.responseStatus . Wreq.statusCode
      body = res ^. Wreq.responseBody
  case rcode of
    200 -> pure . (== 1) . readRes $ body
    _ -> E.throwIO $ ConnectFailure
  where
    req =
      WreqS.getWith opts sess 
        (intercalate "/" ["https://" ++ serverName' ++ ":" ++ show leaderBoardPort', "lowestScoreQuery", show s])

    readRes = fromMaybe 0 . readMaybe @Int . BL8.unpack . BL.take 3 

leaderBoardRequest :: PageNumber -> PageHeight -> WreqS.Session -> IO [(Int, ScoreField)]
leaderBoardRequest (PageNumber pix) (PageHeight psize) sess = do
    res <- req
    let rcode = res ^. Wreq.responseStatus . Wreq.statusCode
        body = res ^. Wreq.responseBody
    case rcode of
      200 -> pure . zip indices . read @[ScoreField] . BL8.unpack $ body
      _ -> E.throwIO $ ConnectFailure
  where
    req =
      WreqS.getWith opts sess $ intercalate "/"
        [ "https://" ++ serverName' ++ ":" ++ show leaderBoardPort'
            ,"leaderboardQuery"
            ,show pix
            , show psize
        ]
    indices = concatMap f pns
    pns = [max 0 (pix - 2) .. pix + 2]
    f p = [p*psize .. p*psize + psize - 1]  

postScoreLeaderBoard :: NameType -> ScoreType -> SeedType -> EventList -> WreqS.Session -> IO ()
postScoreLeaderBoard name score seed evlist sess = wrapRequest () $ do
  res <- req'
  let rcode = res ^. Wreq.responseStatus . Wreq.statusCode
  print res
  print rcode
  case rcode of
    201 -> pure ()
    a -> print a >> error "Failed to upload score"
  where
    req' =
      WreqS.postWith opts sess 
        (intercalate "/" ["https://" ++ serverName' ++ ":" ++ show leaderBoardPort', "addScore", T.unpack name, show score, show seed])
        (converted :: BL.ByteString)

    converted = runPut (mapM_ gameEvPutter evlist)
    gameEvPutter (GameEvent t ev) = do
      maybe (putLazyByteString BL.empty) putWord8 (BM.lookup ev keyEvBytesMap)
      putWord16be (fromIntegral t)