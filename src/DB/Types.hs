{-#LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module DB.Types where

import Control.Exception(Exception)
import Data.Text(Text)
import qualified Data.Text as Text
import Data.ByteString.Lazy(ByteString)
import Data.Word(Word8, Word16)
import GameLogic (ScoreType)
import Network.Socket(Socket)
import qualified Data.IntMap as IMap
import UI.Types
import Database.SQLite.Simple
import GHC.Generics (Generic)
import Data.Bimap (Bimap)
import qualified Data.Bimap as BM
import qualified Data.ByteString.Lazy as B
import Control.Concurrent.STM.TQueue (TQueue)

type Score = ScoreType

type Name = Text

type Time = Int

data ScoreField = ScoreField
  { getScoreFieldID :: !Int,
    getScoreFieldName :: !Name,
    getScoreFieldScore :: !Score,
    getScoreFieldTime :: !Time,
    getSeed :: Maybe SeedType,
    getReplay :: Maybe EventListMessage
  } deriving Generic
    deriving FromRow

instance ToRow ScoreField where
  toRow (ScoreField scoreID name score time Nothing Nothing) =
    toRow (scoreID, name, score, time)
  toRow (ScoreField scoreID name score time (Just seed) (Just replay)) =
    toRow (scoreID, name, score, time, seed, replay)
  toRow _ = error "Not possible!"

instance Eq ScoreField where
  (ScoreField a _ x _ s0 _) == (ScoreField b _ y _ s1 _) = a == b && x == y && s0 == s1

instance Ord ScoreField where
  (ScoreField _ _ s d _ _) <= (ScoreField _ _ s' d' _ _) = s <= s' && d < d'

type MsgLenRep = Word16

newtype TCPConn = TCPConn {getSocket :: Socket}
  deriving newtype (Show)

data ServerState = ServerState {clientCount :: !Int, clients :: !ClientMap, currentIx :: !CIndex}
  deriving (Show)

type DBSize = Word8

type ClientMap = IMap.IntMap TCPConn

type CIndex = Int

type ClientConnection = TCPConn

data ServerStateError
  = ConnectFailure
  | MaxPlayers
  | UnexpectedClose
  | WrongHello
  | HelloTooSlow !ClientConnection
  | OversizedMessage !Int
  deriving stock (Show)
  deriving Exception

data ClientError 
  = NoReplayData
  | DataTimeout
  deriving stock (Show, Eq)
  deriving Exception


-- | A way to mark which kind of bytestring message can be produced.
type BSMessage a = ByteString

type TextMessage a = Text.Text

type SeedMessage = BSMessage SeedType

type ScoreMessage = BSMessage ScoreType

type EventListMessage = BSMessage EventList

type NameMessage = BSMessage Name

data ReplayData = ReplayData SeedType EventListMessage 
  deriving Generic

instance FromRow ReplayData where

keyEvBytesMap :: Bimap KeyEvent ByteString
keyEvBytesMap =
  BM.fromList
    [ (GameStarted, B.singleton 250),
      (MoveUp, B.singleton 251),
      (MoveDown, B.singleton 252),
      (MoveLeft, B.singleton 253),
      (MoveRight, B.singleton 254),
      (GameEnded, B.singleton 255)
    ]

type ThreadPool = TQueue ()


