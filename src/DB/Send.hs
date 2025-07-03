{-# LANGUAGE TypeApplications #-}

module DB.Send where

import Control.Concurrent.STM (STM)
import Control.Concurrent.STM.TQueue (TQueue, writeTQueue)
import qualified DB.Authenticate as Auth
import DB.Types
import Data.Binary (encode)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import GameLogic (ScoreType)
import Network.Socket (gracefulClose)
import Network.Socket.ByteString.Lazy (sendAll)
import UI.Types
import qualified Data.Bimap as BM
import Data.Maybe (fromMaybe)

sendBSMessage :: TCPConn -> BSMessage a -> IO ()
sendBSMessage tcpConn msg =
  sendAll (getSocket tcpConn) $
    encode @MsgLenRep (fromIntegral $ BL.length msg) <> msg

queueBSMessage :: a -> (a -> BSMessage a) -> TQueue (BSMessage a) -> STM ()
queueBSMessage v fv = flip writeTQueue (fv v)

sendSeedMessage :: TCPConn -> SeedType -> IO ()
sendSeedMessage c = sendBSMessage c . seedToMessage
  where
    seedToMessage :: SeedType -> SeedMessage
    seedToMessage = encode

sendScoreMessage :: TCPConn -> ScoreType -> IO ()
sendScoreMessage c = sendBSMessage c . scoreToMessage
  where
    scoreToMessage :: ScoreType -> ScoreMessage
    scoreToMessage = encode

sendEventList :: TCPConn -> EventList -> IO ()
sendEventList c = sendBSMessage c . B.concat . map gameEvToMessage
  where
    gameEvToMessage :: GameEvent -> BSMessage GameEvent
    gameEvToMessage (GameEvent tn ev) = looked <> tickNoToBytes tn
      where
        looked = fromMaybe B.empty (BM.lookup ev keyEvBytesMap)
        tickNoToBytes (TickNumber tno) = encode tno


sendName :: TCPConn -> Name -> IO ()
sendName c = sendTextMessage c . nameToMessage
  where
    nameToMessage = id

sendHello :: TCPConn -> IO ()
sendHello c = sendBSMessage c Auth.helloMessage

sendReplayData :: TCPConn -> ReplayData -> IO ()
sendReplayData c (ReplayData s evs) =
  pure <$> sendSeedMessage c s <*> sendBSMessage c evs

sendTextMessage :: TCPConn -> Text.Text -> IO ()
sendTextMessage tcpConn msg =
  let txtMessage = BL.fromStrict $ Text.encodeUtf8 msg
   in sendAll (getSocket tcpConn) $
        encode @MsgLenRep (fromIntegral $ Text.length msg) <> txtMessage

closeConn :: TCPConn -> IO ()
closeConn conn = gracefulClose (getSocket conn) 500

sendScoreFieldID :: TCPConn -> Int -> IO ()
sendScoreFieldID c = 
  sendBSMessage c . encode

