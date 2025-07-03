{-# LANGUAGE TypeApplications #-}

module DB.Receive where

import qualified Control.Exception as E
import Control.Monad (when)
import DB.Types
import qualified Data.Bimap as BM
import Data.Binary (decode)
import Data.Binary.Get
import Data.Bits (finiteBitSize)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Internal as BL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Logging.Replay (Seed)
import Network.Socket.ByteString.Lazy (recv)
import System.Random.Stateful (mkStdGen)
import UI.Types

lenBytes :: Int
lenBytes = fromIntegral $ finiteBitSize @MsgLenRep 0 `div` 8

--- Receiving Functions

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