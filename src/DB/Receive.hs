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
import DB.Types
import qualified Data.Bimap as BM
import Data.Binary (decode)
import Data.Binary.Get
import Data.Bits (finiteBitSize)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Internal as BL
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Logging.Replay (Seed)
import Network.Socket (close)
import Network.Socket.ByteString.Lazy (recv)
import System.Random.Stateful (mkStdGen)
import UI.Types
import Network.TLS (recvData, Context, bye)

lenBytes :: Int
lenBytes = fromIntegral $ finiteBitSize @MsgLenRep 0 `div` 8

class RecvData a where
  recvInfo :: a -> (BSMessage b -> b) -> IO b

instance RecvData TLSConn where
  recvInfo = recvTLS

instance RecvData TCPConn where
  recvInfo = recvTCPData

--- Receiving Functions

recvTLS ::  TLSConn -> (BSMessage a -> a) -> IO a
recvTLS (TLSConn c) hdlr = hdlr . BS.fromStrict <$> recvData c

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
          rest <- recvAll tcpConn missing
          pure $ bs <> rest
        else pure bs

-------

handleEventList :: EventListMessage -> EventList
handleEventList = go decoder
  where
    getGE = do
      ev <- getWord8
      t <- getWord16be
      return (GameEvent (TickNumber t) (keyEvBytesMap BM.!> BL.singleton ev))
    decoder = runGetIncremental getGE
    go (Done rest _con ev) inp0 =
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

validateHello :: ThreadPool -> TChan Text -> TLSConn -> TCPConn -> (TLSConn -> IO ()) -> IO ()
validateHello threadPool messageChan tlsConn cliConn action = E.handle recvHandler $ 
  flip E.finally (atomically $ writeTQueue threadPool ()) $ do
    _ <- atomically $ readTQueue threadPool
    initHandshake <- race (threadDelay 2_000_000) (recvTLS tlsConn id)
    either
      ( const $
          textWriteTChan messageChan "Nothing received, closing"
            >> bye (getCtx tlsConn)
      )
      (flip validateHelloBytes action)
      initHandshake
  where
    validateHelloBytes bytes act
      | bytes /= Auth.helloMessage = do
          textWriteTChan messageChan $ "Wrong hello received from: " <> show (getSocket cliConn)
          E.throwIO WrongHello
      | otherwise = act tlsConn
    
    recvHandler UnexpectedClose = do
      textWriteTChan messageChan $ "Unexpected closure - lost connection with: " <> show (getSocket cliConn)
    recvHandler WrongHello = do
      textWriteTChan messageChan $ "Wrong Hello received from: " <> show (getSocket cliConn) 
    recvHandler e = E.throwIO e