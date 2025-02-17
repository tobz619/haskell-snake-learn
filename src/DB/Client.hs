{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module DB.Client where

import DB.Highscores (Name)
import Data.Bimap (Bimap)
import qualified Data.Bimap as BM
import Data.Binary (encode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import GameLogic (ScoreType)
import Logging.Logger (EventList, GameEvent (..), TickNumber (..), TickType)
import Logging.Replay (Seed)
import qualified Network.WebSockets as WS
import UI.Gameplay (SeedSize)
import UI.Keybinds (KeyEvent (..))
import qualified Web.Scotty as S

newtype Client = Client {sConn :: WS.Connection}

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

-- keyEvBytesMap MoveUp = B.singleton 251
-- keyEvBytesMap MoveDown = B.singleton 252
-- keyEvBytesMap MoveLeft = B.singleton 253
-- keyEvBytesMap MoveRight = B.singleton 254
-- keyEvBytesMap GameEnded = B.singleton 255
-- keyEvBytesMap _ = B.empty




gameEvToMessage :: GameEvent -> BSMessage GameEvent
gameEvToMessage (GameEvent tn ev) = looked <> tickNoToBytes tn
  where
    looked = fromMaybe B.empty (BM.lookup ev keyEvBytesMap)
    tickNoToBytes (TickNumber tno) = encode tno

type SeedMessage = BSMessage SeedSize

sendSeedMessage :: WS.Connection -> SeedSize -> IO ()
sendSeedMessage c = WS.sendBinaryData c . seedToMessage
  where
    seedToMessage :: SeedSize -> SeedMessage
    seedToMessage = encode

type ScoreMessage = BSMessage ScoreType

sendScoreMessage :: WS.Connection -> ScoreType -> IO ()
sendScoreMessage c = WS.sendBinaryData c . scoreToMessage
  where
    scoreToMessage :: ScoreType -> ScoreMessage
    scoreToMessage = encode

type EventListMessage = BSMessage EventList

sendEventList :: WS.Connection -> EventList -> IO ()
sendEventList c = WS.sendBinaryDatas c . map gameEvToMessage

closeConn :: WS.Connection -> IO ()
closeConn conn = WS.sendClose conn ("Closing connection" :: ByteString)

type NameMessage = TextMessage Name

sendName :: WS.Connection -> Name -> IO ()
sendName c = WS.sendTextData c . nameToMessage
  where
    nameToMessage = id