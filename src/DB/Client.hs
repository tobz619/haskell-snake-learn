{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module DB.Client where

import Control.Concurrent.Async
-- import Data.Int (Int64)

import Control.Concurrent.STM (STM, TQueue, atomically, flushTQueue, newTQueueIO, writeTQueue)
import qualified Control.Exception as E
import DB.Highscores (Name)
import qualified DB.Authenticate as Auth
import Data.Bimap (Bimap)
import qualified Data.Bimap as BM
import Data.Binary (encode)
import Data.Bits (FiniteBits (finiteBitSize))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.List (scanl')
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Word (Word8)
import GameLogic (ScoreType)
import Logging.Logger (EventList, GameEvent (..), TickNumber (..))
import Network.Socket
import Network.Socket.ByteString.Lazy (sendAll)
import Network.TLS
import System.Random (mkStdGen)
import UI.Gameplay (SeedType)
import UI.Keybinds (KeyEvent (..))

type MsgLenRep = Word8

newtype TCPConn = TCPConn {getSocket :: Socket}
  deriving newtype (Show)

serverName, serverName' :: HostName
serverName = "127.0.0.1"
serverName' = "haskell-server.tobioloke.com"

clientPort, clientPort' :: PortNumber
clientPort = 34561
clientPort' = 5000

lenBytes :: Int
lenBytes = fromIntegral $ finiteBitSize @MsgLenRep 0 `div` 8

-- | A way to mark which kind of bytestring message can be produced.
type BSMessage a = ByteString

type TextMessage a = Text.Text

keyEvBytesMap :: Bimap KeyEvent ByteString
keyEvBytesMap =
  foldr
    (uncurry BM.insert)
    BM.empty
    [ (MoveUp, B.singleton 251),
      (MoveDown, B.singleton 252),
      (MoveLeft, B.singleton 253),
      (MoveRight, B.singleton 254),
      (GameEnded, B.singleton 255)
    ]

gameEvToMessage :: GameEvent -> BSMessage GameEvent
gameEvToMessage (GameEvent tn ev) = looked <> tickNoToBytes tn
  where
    looked = fromMaybe B.empty (BM.lookup ev keyEvBytesMap)
    tickNoToBytes (TickNumber tno) = encode tno

type SeedMessage = BSMessage SeedType

sendSeedMessage :: TCPConn -> SeedType -> IO ()
sendSeedMessage c = sendBSMessage c . seedToMessage
  where
    seedToMessage :: SeedType -> SeedMessage
    seedToMessage = encode

type ScoreMessage = BSMessage ScoreType

sendScoreMessage :: TCPConn -> ScoreType -> IO ()
sendScoreMessage c = sendBSMessage c . scoreToMessage
  where
    scoreToMessage :: ScoreType -> ScoreMessage
    scoreToMessage = encode

type EventListMessage = BSMessage EventList

sendEventList :: TCPConn -> EventList -> IO ()
sendEventList c = sendBSMessage c . B.concat . map gameEvToMessage

sendBSMessage :: TCPConn -> BSMessage a -> IO ()
sendBSMessage tcpConn msg =
  sendAll (getSocket tcpConn) $
    encode @MsgLenRep (fromIntegral $ B.length msg) <> msg

queueBSMessage :: a -> (a -> BSMessage a) -> TQueue ByteString -> STM ()
queueBSMessage v fv = flip writeTQueue (fv v)

sendTextMessage :: TCPConn -> Text.Text -> IO ()
sendTextMessage tcpConn msg =
  let txtMessage = B.fromStrict $ Text.encodeUtf8 msg
   in sendAll (getSocket tcpConn) $
        encode @MsgLenRep (fromIntegral $ Text.length msg) <> txtMessage

closeConn :: TCPConn -> IO ()
closeConn conn = gracefulClose (getSocket conn) 500

type NameMessage = BSMessage Name

sendName :: TCPConn -> Name -> IO ()
sendName c = sendTextMessage c . nameToMessage
  where
    nameToMessage = id

sendHello :: TCPConn -> IO ()
sendHello c = sendBSMessage c Auth.helloMessage

runClientAppSTM :: SeedType -> ScoreType -> Text.Text -> [GameEvent] -> IO ()
runClientAppSTM seed score name evList = withSocketsDo $ do
  q <- newTQueueIO
  runTCPClient serverName' clientPort' (app q)
  where
    app tq c = do
      actions <- atomically $ do
        writeTQueue tq $ putStrLn $ "Sending hello message"
        writeTQueue tq $ sendHello c
        writeTQueue tq $ putStrLn $ "Sending seed: " ++ show (mkStdGen 4)
        writeTQueue tq $ sendScoreMessage c score
        writeTQueue tq $ sendName c name
        writeTQueue tq $ sendSeedMessage c seed
        writeTQueue tq $ sendEventList c evList
        writeTQueue tq $ putStrLn "Closing conn"
        flushTQueue tq
      sequence_ actions

runTCPClient :: HostName -> PortNumber -> (TCPConn -> IO b) -> IO b
runTCPClient hostName port action = flip withAsync wait $ do
  addr <- resolve
  E.bracketOnError (connectClientTCPSocket addr) closeConn action
  where
    resolve = do
      let hints = defaultHints {addrSocketType = Stream}
      NE.head <$> getAddrInfo (pure hints) (pure hostName) (pure (show port))

    connectClientTCPSocket addr = E.bracketOnError
      (socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr))
      close
      $ \sock -> do
        print $ addrAddress addr
        setSocketOption sock NoDelay 1
        setSocketOption sock Linger 5

        connect sock $ addrAddress addr

        pure $ TCPConn sock

testClient :: IO ()
testClient =
  let moves = [1, 3, 8, 10, 1, 11, 14, 1, 1]
      events =
        zipWith
          (GameEvent . TickNumber)
          (scanl' (+) 1 moves)
          [MoveRight, MoveDown, MoveLeft, MoveUp, MoveRight, MoveDown, MoveRight, MoveDown, MoveLeft]
   in do
        runClientAppSTM 4 4 ("BOB" :: Name) events

manyTestClients :: Int -> IO ()
manyTestClients n = mapM_ (\(v, act) -> act >> print v) $ zip ([1 ..] :: [Int]) (replicate n testClient)

-- repTest = do
--  evs <- newMVar events
--  runReplayApp (mkStdGen 4) evs