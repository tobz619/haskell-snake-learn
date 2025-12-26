{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module DB.Types where

import Control.Exception (Exception)
import Data.Bimap (Bimap)
import qualified Data.Bimap as BM
import Data.ByteString.Lazy (ByteString)
import qualified Data.IntMap as IMap
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word16, Word8)
import Database.SQLite.Simple
import GHC.Generics (Generic)
import GameLogic (ScoreType)
import Network.Socket (Socket)
import UI.Types
import Network.TLS (Context)
import Control.Concurrent.STM.TSem (TSem)
import qualified Data.ByteString.Lazy.Char8 as B8
import Network.HTTP.Media (MediaType, (//), (/:))
import Data.Data (Proxy)
import Text.Read (readEither)

type Score = ScoreType

type NameType = Text

type Time = Int

newtype PageNumber = PageNumber Int
  deriving newtype (Show, Eq, Num)

newtype PageHeight = PageHeight Int
  deriving newtype (Show, Eq, Num)


data ScoreField = ScoreField
  { getScoreFieldID :: !Int,
    getScoreFieldName :: !NameType,
    getScoreFieldScore :: !Score,
    getScoreFieldTime :: !Time,
    getSeed :: Maybe SeedType,
    getReplay :: Maybe EventListMessage
  }
  deriving stock (Show, Read, Generic)
  deriving anyclass FromRow

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

newtype TLSConn = TLSConn {getCtx :: Context}

data ServerState = ServerState {clientCount :: !Int, clients :: !ClientMap, currentIx :: !CIndex}
  deriving (Show)

type DBSize = Word8

type ClientMap = IMap.IntMap TCPConn

type CIndex = Int

type ClientConnection = TCPConn


data ServerStateError
  = ConnectTimeout
  | MaxPlayers
  | UnexpectedClose
  | WrongHello
  | HelloTooSlow
  | OversizedMessage !Int
  | MalformedEvents
  deriving stock (Show)
  deriving anyclass (Exception)

data ClientError
  = NoReplayData
  | DataTimeout
  deriving stock (Show, Eq)
  deriving (Exception)

-- | A way to mark which kind of bytestring message can be produced.
type BSMessage a = ByteString

type TextMessage a = Text.Text

type SeedMessage = BSMessage SeedType

type ScoreMessage = BSMessage ScoreType

type EventListMessage = BSMessage EventList

type NameMessage = BSMessage NameType

data ReplayData = ReplayData SeedType EventListMessage
  deriving (Generic)

instance FromRow ReplayData

keyEvBytesMap :: Bimap KeyEvent Word8
keyEvBytesMap =
  BM.fromList
    [ (GameStarted, 250),
      (MoveUp, 251),
      (MoveDown, 252),
      (MoveLeft, 253),
      (MoveRight, 254),
      (GameEnded, 255)
    ]

type ThreadPool = TSem
