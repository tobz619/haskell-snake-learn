{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module DB.Client where


import Control.Concurrent.Async
import Control.Monad (replicateM_)
import DB.Highscores (Name)
import Data.Bimap (Bimap)
import qualified Data.Bimap as BM
import Data.Bits (FiniteBits (finiteBitSize))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Char8 as B8 
import qualified Data.ByteString.Lazy as B
-- import Data.Int (Int64)
import Data.List (scanl')
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as TextL
import Data.Word (Word32, Word8)
import GHC.Conc
import GHC.Generics (Generic)
import GHC.IO (finally, mask)
import GameLogic (ScoreType)
import Logging.Logger (EventList, GameEvent (..), TickNumber (..))
import Network.Socket
import Network.Socket.ByteString.Lazy ( sendAll )
import Network.TLS
import System.Random (mkStdGen)
import UI.Gameplay (SeedSize)
import UI.Keybinds (KeyEvent (..))
import UI.ReplayPlayer (runReplayApp)
import Wuss (runSecureClient, runSecureClientWith)
import qualified Data.List.NonEmpty as NE
import Data.Text.Encoding (encodeUtf8)
import Data.Binary (encode)
import Data.Coerce (coerce)
import Unsafe.Coerce (unsafeCoerce)

type MsgLenRep = Word8

newtype TCPConn = TCPConn {getSocket :: Socket}

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

type SeedMessage = BSMessage SeedSize

sendSeedMessage :: TCPConn -> SeedSize -> IO ()
sendSeedMessage c = sendBSMessage c . seedToMessage
  where
    seedToMessage :: SeedSize -> SeedMessage
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

sendBSMessage :: TCPConn -> ByteString -> IO ()
sendBSMessage tcpConn msg =
  sendAll (getSocket tcpConn) $
    encode @MsgLenRep (fromIntegral $ B.length msg) <> msg

sendTextMessage :: TCPConn -> Text.Text -> IO ()
sendTextMessage tcpConn msg =
  let txtMessage = B.fromStrict $ Text.encodeUtf8 msg
  in sendAll (getSocket tcpConn) $
      encode @MsgLenRep (fromIntegral $ Text.length msg) <> txtMessage

-- sendEventList c = let
--  evListSize = length evlist
--  in do
-- WS.sendBinaryData c (B.singleton $ fromIntegral evListSize)

closeConn :: TCPConn -> IO ()
closeConn conn = gracefulClose (getSocket conn) 500

type NameMessage = BSMessage Name

sendName :: TCPConn -> Name -> IO ()
sendName c = sendTextMessage c . nameToMessage
  where
    nameToMessage = id

runClientApp :: SeedSize -> ScoreType -> Text.Text -> [GameEvent] -> IO ()
runClientApp seed score name evList =
  -- withSocketsDo $ runTCPClient "127.0.0.1" 34561 app
  withSocketsDo $ runTCPClient "haskell-server.tobioloke.com" 443 app
  where
    app c = do
      sendScoreMessage c score
      sendName c name
      sendSeedMessage c seed
      sendEventList c evList
      closeConn c
    
    -- ackhdlr = TL.decodeUtf8

runTCPClient :: HostName -> PortNumber -> (TCPConn -> IO b) -> IO b
runTCPClient hostName port action = mask $ \restore -> do
  addr <- resolve
  tcpSock <- withAsync (connectClientTCPSocket addr) $ \sock -> wait sock
  restore (action tcpSock) `finally` closeConn tcpSock
  where
    resolve = do
      let hints = defaultHints {addrSocketType = Stream }
      NE.head <$> getAddrInfo (Just hints) (Just hostName) (Just (show port))
    
    connectClientTCPSocket addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      setSocketOption sock NoDelay 1
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
        putStrLn $ "Sending seed: " ++ show (mkStdGen 4)
        -- 
        runClientApp 4 4 ("MAX" :: Name) events

-- repTest = do
--  evs <- newMVar events
--  runReplayApp (mkStdGen 4) evs