module DB.Client where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word8)
import Logging.Logger (GameEvent (..), TickNumber (..), EventList)
import Logging.Replay
import qualified Network.WebSockets as WS
import qualified Web.Scotty as S
import UI.Keybinds (KeyEvent (..))

newtype Client = Client {sConn :: WS.Connection}

keyEvToBytes :: KeyEvent -> ByteString
keyEvToBytes MoveUp = B.singleton 251
keyEvToBytes MoveDown = B.singleton 252
keyEvToBytes MoveLeft = B.singleton 253
keyEvToBytes MoveRight = B.singleton 254
keyEvToBytes GameEnded = B.singleton 255
keyEvToBytes _ = B.empty

tickNoToBytes :: TickNumber -> ByteString
tickNoToBytes (TickNumber tn) = B.pack [hi, lo]
  where
    (hi, lo) = quotRem (fromIntegral tn) (maxBound :: Word8)

gameEvToMessage :: GameEvent -> WS.DataMessage
gameEvToMessage (GameEvent tn ev) = WS.Binary $ keyEvToBytes ev <> tickNoToBytes tn

sendEventList :: Client -> EventList -> IO ()
sendEventList c = WS.sendDataMessages (sConn c) . map gameEvToMessage

closeConn = undefined