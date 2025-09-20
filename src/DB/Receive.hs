{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use section" #-}

module DB.Receive where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad (when)
import qualified DB.Authenticate as Auth
import DB.Send
import DB.Types
import qualified Data.Bimap as BM
import Data.Binary (decode)
import Data.Binary.Get
import Data.Bits (finiteBitSize)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Internal as BL
import Data.Coerce (coerce)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Logging.Replay (Seed)
import Network.Socket.ByteString.Lazy (recv)
import Network.TLS (TLSException, contextClose, handshake, recvData)
import System.Random.Stateful (mkStdGen)
import UI.Types
import Network.Socket
import Control.Concurrent.STM.TSem (waitTSem, signalTSem)

lenBytes :: Int
lenBytes = fromIntegral $ finiteBitSize @MsgLenRep 0 `div` 8

class RecvData a where
  recvInfo :: a -> (BSMessage b -> b) -> IO b

instance RecvData TLSConn where
  recvInfo = recvTLS

instance RecvData TCPConn where
  recvInfo = recvTCPData

--- Receiving Functions

recvTLS :: TLSConn -> (BSMessage a -> a) -> IO a
recvTLS (TLSConn c) hdlr = hdlr . BL.fromStrict <$> recvData c

recvTCPData :: TCPConn -> (BSMessage b -> b) -> IO b
recvTCPData tcpConn handler = do
  lenPrefixBytes <- recvAll tcpConn (fromIntegral lenBytes)
  let msglen = (fromIntegral . decode @MsgLenRep) lenPrefixBytes
  when (msglen > (maxBound :: MsgLenRep)) (E.throwIO $ OversizedMessage (fromIntegral msglen))
  handler <$> recvAll tcpConn msglen

recvAll :: TCPConn -> MsgLenRep -> IO BL.ByteString
recvAll tcpConn size
  | size <= 0 = pure BL.empty
  | otherwise = do
      bs <- recv (getSocket tcpConn) (fromIntegral size)
      when (BL.null bs) $ E.throwIO UnexpectedClose
      let len = BL.length bs
      if len < fromIntegral size
        then do
          let missing = size - fromIntegral len
          (bs <>) <$> recvAll tcpConn missing
        else pure bs

-------

handleEventList :: EventListMessage -> EventList
handleEventList = go decoder
  where
    getGE = do
      ev <- getWord8
      t <- getWord16be
      return (GameEvent (TickNumber t) (keyEvBytesMap BM.!> ev))
    decoder = runGetIncremental getGE
    go (Done rest _ ev) inp0 =
      ev : go decoder (BL.Chunk rest inp0)
    go (Partial k) inp = case inp of
      BL.Chunk h t -> go (k $ Just h) t
      BL.Empty -> []
    go (Fail _ _ msg) _ =
      error msg


messageToSeed :: SeedMessage -> Seed
messageToSeed = mkStdGen . decode

messageToScore :: ScoreMessage -> Score
messageToScore = decode

messageToName :: NameMessage -> Name
messageToName = TL.toStrict . TL.decodeUtf8

textWriteTChan :: TChan Text -> String -> IO ()
textWriteTChan c = atomically . writeTChan c . Text.pack

messageToScoreID :: BSMessage Int -> Int
messageToScoreID = decode @Int

validateHello :: ThreadPool -> TChan Text -> TLSConn -> TCPConn -> IO () -> IO ()
validateHello threadPool messageChan tlsConn cliConn action = do
  E.handle recvHandler $ do
    initHandshake <- race (threadDelay 5_000_000) (E.handle handshakeHandler $ handshake (coerce tlsConn))
    -- \^ Make sure the handshake happens within two seconds of connecting.
    either
      ( const $
          textWriteTChan messageChan "Nothing received, closing"
            >> E.throwIO HelloTooSlow
            -- >> requestClose cliConn
      )
      (\_ -> E.handle handshakeHandler $ (recvInfo tlsConn id >>= validateHelloBytes))
      initHandshake
  where
    -- E.handle handshakeHandler $ handshake (coerce tlsConn)
    -- recvInfo tlsConn id >>= validateHelloBytes

    validateHelloBytes bytes
      | bytes /= Auth.helloMessage = E.throwIO WrongHello
      | otherwise =
          E.bracket_
            (atomically $ waitTSem threadPool) -- Take thread out the pool
            (atomically $ signalTSem threadPool) -- Put it back when done or even if the computation fails
            action

    recvHandler UnexpectedClose = do
      textWriteTChan messageChan $ "Unexpected closure - lost connection with: " <> show (getSocket cliConn)
    recvHandler WrongHello = do
      textWriteTChan messageChan $ "Wrong Hello received from: " <> show (getSocket cliConn)
    recvHandler HelloTooSlow = do
      sockAddr <- getPeerName $ coerce cliConn
      textWriteTChan messageChan $ "Hello too slow from: " <> show sockAddr
    recvHandler e = pure ()

    handshakeHandler :: TLSException -> IO ()
    -- handshakeHandler _ = E.throwIO WrongHello
    handshakeHandler _ = E.throwIO WrongHello
