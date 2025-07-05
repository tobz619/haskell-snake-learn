{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module DB.Send where

import Control.Concurrent.STM (STM)
import Control.Concurrent.STM.TQueue (TQueue, writeTQueue)
import qualified DB.Authenticate as Auth
import DB.Types
import qualified Data.Bimap as BM
import Data.Binary (encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import GameLogic (ScoreType)
import Network.Socket (gracefulClose)
import Network.Socket.ByteString.Lazy (sendAll)
import Network.TLS
import UI.Types


class SendData a where
  sendBSMessage :: a -> BSMessage a -> IO () 
  sendTextMessage :: a -> TextMessage a -> IO ()

instance SendData TCPConn where
  sendBSMessage = sendBSMessageTCP
  sendTextMessage = sendTextMessageTCP

instance SendData TLSConn where
  sendBSMessage = sendBSMessageTLS
  sendTextMessage = sendTextMessageTLS

sendBSMessageTLS :: TLSConn -> BSMessage a -> IO ()
sendBSMessageTLS (TLSConn c) = sendData c

sendTextMessageTLS :: TLSConn -> Text.Text -> IO ()
sendTextMessageTLS tlsConn txt = 
  let txtMessage = BL.fromStrict . Text.encodeUtf8 $ txt
  in sendBSMessageTLS tlsConn txtMessage

sendBSMessageTCP :: TCPConn -> BSMessage a -> IO ()
sendBSMessageTCP tcpConn msg =
  sendAll (getSocket tcpConn) $
    encode @MsgLenRep (fromIntegral $ BL.length msg) <> msg

queueBSMessage :: a -> (a -> BSMessage a) -> TQueue (BSMessage a) -> STM ()
queueBSMessage v fv = flip writeTQueue (fv v)

sendSeedMessage :: SendData b => b -> SeedType -> IO ()
sendSeedMessage c = sendBSMessage c . seedToMessage
  where
    seedToMessage :: SeedType -> SeedMessage
    seedToMessage = encode

sendScoreMessage :: SendData b => b -> ScoreType -> IO ()
sendScoreMessage c = sendBSMessage c . scoreToMessage
  where
    scoreToMessage :: ScoreType -> ScoreMessage
    scoreToMessage = encode

sendEventList :: SendData b => b -> EventList -> IO ()
sendEventList c = sendBSMessage c . BL.concat . map gameEvToMessage
  where
    gameEvToMessage :: GameEvent -> BSMessage GameEvent
    gameEvToMessage (GameEvent tn ev) = looked <> tickNoToBytes tn
      where
        looked = fromMaybe BL.empty (BM.lookup ev keyEvBytesMap)
        tickNoToBytes (TickNumber tno) = encode tno

sendName :: SendData b => b -> Name -> IO ()
sendName c = sendTextMessage c . nameToMessage
  where
    nameToMessage = id

sendHello :: SendData b => b -> IO ()
sendHello c = sendBSMessage c Auth.helloMessage

sendReplayData :: SendData b => b -> ReplayData -> IO ()
sendReplayData c (ReplayData s evs) =
  pure <$> sendSeedMessage c s <*> sendBSMessage c evs

sendTextMessageTCP :: SendData b => b -> Text.Text -> IO ()
sendTextMessageTCP tcpConn msg =
  let txtMessage = BL.fromStrict $ Text.encodeUtf8 msg
   in sendBSMessage tcpConn txtMessage

closeConn :: TCPConn -> IO ()
closeConn conn = gracefulClose (getSocket conn) 500

sendScoreFieldID :: SendData b => b -> Int -> IO ()
sendScoreFieldID c =
  sendBSMessage c . encode
