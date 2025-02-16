{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module DB.Client where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Bimap (Bimap)
import qualified Data.Bimap as BM
import qualified Data.Text as Text
import Data.Word (Word8, Word64)
import Logging.Logger (GameEvent (..), TickNumber (..), EventList)
import qualified Network.WebSockets as WS
import qualified Web.Scotty as S
import UI.Keybinds (KeyEvent (..))
import Data.Maybe (fromMaybe)
import Logging.Replay (Seed)

newtype Client = Client {sConn :: WS.Connection}

type BSMessage a = ByteString
type TextMessage a = Text.Text


keyEvBytesMap :: Bimap KeyEvent ByteString
keyEvBytesMap = foldr (uncurry BM.insert) BM.empty [
  (MoveUp, B.singleton 251),
  (MoveDown, B.singleton 252),
  (MoveLeft, B.singleton 253),
  (MoveRight, B.singleton 254),
  (GameEnded, B.singleton 255)
  ]
-- keyEvBytesMap MoveUp = B.singleton 251
-- keyEvBytesMap MoveDown = B.singleton 252
-- keyEvBytesMap MoveLeft = B.singleton 253
-- keyEvBytesMap MoveRight = B.singleton 254
-- keyEvBytesMap GameEnded = B.singleton 255
-- keyEvBytesMap _ = B.empty

tickNoToBytes :: TickNumber -> ByteString
tickNoToBytes (TickNumber tn) = B.pack [hi, lo]
  where
    (hi, lo) = quotRem (fromIntegral tn) (maxBound :: Word8)

gameEvToMessage :: GameEvent -> BSMessage GameEvent
gameEvToMessage (GameEvent tn ev) = looked <> tickNoToBytes tn
  where looked = fromMaybe B.empty (BM.lookup ev keyEvBytesMap)

seedToMessage :: Client -> Word64 -> IO ()
seedToMessage c = WS.sendTextData (sConn c) . Text.pack . show

sendEventList :: Client -> EventList -> IO ()
sendEventList c = WS.sendBinaryDatas (sConn c) . map gameEvToMessage

closeConn :: WS.Connection -> IO ()
closeConn conn = WS.sendClose conn ("Closing connection" :: ByteString) 