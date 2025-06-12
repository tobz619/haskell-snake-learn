{-#LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies #-}
module DB.Types where

import Control.Exception(Exception)
import Data.Text(Text)
import qualified Data.Text as Text
import Data.ByteString.Lazy(ByteString)
import Data.Word(Word8)
import GameLogic (ScoreType)
import Network.Socket(Socket)
import qualified Data.IntMap as IMap
import UI.Types
import Database.SQLite.Simple

type Score = ScoreType

type Name = Text

type Time = Int

data ScoreField = ScoreField
  { getScoreFieldName :: !Name,
    getScoreFieldScore :: !Score,
    getScoreFieldTime :: !Time,
    getReplay :: Maybe EventListMessage
  }

instance FromRow ScoreField where
  fromRow = ScoreField <$> field <*> field <*> field <*> field

instance ToRow ScoreField where
  toRow (ScoreField name score time Nothing) =
    toRow (name, score, time)
  toRow (ScoreField name score time (Just replay)) =
    toRow (name, score, time, replay)

instance Eq ScoreField where
  (ScoreField _ x _ _) == (ScoreField _ y _ _) = x == y

instance Ord ScoreField where
  (ScoreField _ s d _) <= (ScoreField _ s' d' _) = s <= s' && d < d'

type MsgLenRep = Word8

newtype TCPConn = TCPConn {getSocket :: Socket}
  deriving newtype (Show)

data ServerState = ServerState {clientCount :: !Int, clients :: !ClientMap, currentIx :: !CIndex}
  deriving (Show)

type ClientMap = IMap.IntMap TCPConn

type CIndex = Int

type ClientConnection = TCPConn

data ServerStateError
  = ConnectFailure
  | MaxPlayers
  | UnexpectedClose
  | HelloTooSlow !ClientConnection
  deriving stock (Show)

instance Exception ServerStateError

-- | A way to mark which kind of bytestring message can be produced.
type BSMessage a = ByteString

type TextMessage a = Text.Text

type SeedMessage = BSMessage SeedType

type ScoreMessage = BSMessage ScoreType

type EventListMessage = BSMessage EventList
