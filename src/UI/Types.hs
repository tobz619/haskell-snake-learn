{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module UI.Types where

import Bluefin.State (State)
import Brick.Forms (Form)
import qualified Brick.Keybindings as K
import Brick.Widgets.Dialog (Dialog)
import Control.Concurrent.MVar (MVar)
import Data.List (nub)
import qualified Data.Vector.Strict as V
import Data.Word (Word16)
import GameLogic hiding (Stream)
import Lens.Micro.TH (makeLenses)
import Linear.V2 (V2)
import qualified Network.Wreq.Session as WreqS
import Options.Options (Options)

type ConfigBinding = (KeyEvent, K.BindingState)

newtype TickNumber = TickNumber TickType
  deriving newtype (Eq, Show, Read, Num, Ord, Enum, Real, Integral)

type TickType = Word16

newtype EvNumber = EvNumber Int deriving newtype (Eq, Show, Read, Num, Ord)

data KeyEvent = MoveUp | MoveDown | MoveLeft | MoveRight | FoodEaten !(V2 Int) | Back | Select | Pause | GameStarted | GameEnded | Halt | QuitGame
  deriving (Show, Eq, Ord, Read)

-- | Pairing of tick events to significant moves
data GameEvent = GameEvent {gEvTick :: TickNumber, gEvEvent :: KeyEvent}
  deriving (Show, Eq, Read)

-- instance Show GameEvent where
--   show (GameEvent tn ev) = "GameEvent " <> show tn <> " " <> show ev

type EventList = [GameEvent]

type InputList = V.Vector GameEvent

data RewindType = RewindType
  { rwTick :: TickNumber,
    rwEv :: EvNumber,
    rwGS :: GameState
  }
  deriving (Eq)

type RewindBuffer = V.Vector RewindType

mkRewindBuffer :: [RewindType] -> RewindBuffer
mkRewindBuffer = V.fromListN 256 . nub

mkInputList :: [GameEvent] -> InputList
mkInputList = V.fromList

mkEvs :: EventList -> EventList
mkEvs = reverse . filter (\(GameEvent _ x) -> isMovement x)
  where
    isMovement x = x `elem` [MoveUp, MoveDown, MoveLeft, MoveRight]

newtype Logger i e = Logger (State i e)

-- | Marks passing of time.
--  Each delta is fed into the app.
data Tick = Tick

-- | The type of the cell
data Cell = Snake | Food | Empty

data MenuOptions = Resume | Restart | Quit | Yes | No | OpChar Int
  deriving (Show, Eq, Ord)

data GameplayState = GameplayState
  { _gameState :: GameState,
    _gameStateDialog :: Maybe (Dialog GameState MenuOptions),
    _highScoreDialogs :: HighScoreFormState,
    _tickNo :: TickType,
    _gameLog :: EventList,
    _gameSeed :: Maybe SeedType,
    _mode :: GameplayStateMode,
    _opts :: Options
  }

data HighScoreFormState = HighScoreFormState
  { _hsDialog :: Maybe (Dialog HighScoreFormState MenuOptions),
    _hsForm :: Maybe (Form HighScoreForm () MenuOptions)
  }

data GameplayStateMode = StartingMode | GameplayMode | CheckingHS Bool | AskingHS Bool | SubmittingHS Bool
  deriving (Show, Eq)

data HighScoreForm = HighScoreForm {_cha1 :: Maybe Char, _cha2 :: Maybe Char, _cha3 :: Maybe Char}

type SeedType = Int

makeLenses ''GameplayState
makeLenses ''HighScoreForm
makeLenses ''HighScoreFormState