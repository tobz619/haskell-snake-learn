{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module UI.Types where

import GameLogic hiding (Stream)
import Lens.Micro.TH (makeLenses)
import Brick.Widgets.Dialog (Dialog)
import System.Random (StdGen)
import Brick.Forms (Form)
import Data.Word (Word16)
import Bluefin.State (State)
import Linear.V2 (V2)
import qualified Brick.Keybindings as K
import Data.Vector.Strict
import qualified Data.Map as Map

type ConfigBinding = (KeyEvent, K.BindingState)

newtype TickNumber = TickNumber TickType deriving newtype (Eq, Show, Num, Ord)
type TickType = Word16

data KeyEvent = MoveUp | MoveDown | MoveLeft | MoveRight | FoodEaten !(V2 Int) | Back | Select | Pause | GameEnded | Halt | QuitGame
  deriving (Show, Eq, Ord)

-- | Pairing of tick events to significant moves
data GameEvent = GameEvent TickNumber KeyEvent
  deriving (Show)

type EventList = [GameEvent]

type InputList = Vector GameEvent

type CheckPointMap = Map.Map TickNumber GameState

mkInputList :: [GameEvent] -> InputList
mkInputList = fromList

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
    _gameSeed :: (SeedType, StdGen)
  }

data HighScoreFormState = HighScoreFormState
  { _hsDialog :: Maybe (Dialog HighScoreFormState MenuOptions),
    _hsForm :: Maybe (Form HighScoreForm () MenuOptions)
  }

data HighScoreForm = HighScoreForm {_cha1 :: Maybe Char, _cha2 :: Maybe Char, _cha3 :: Maybe Char}

type SeedType = Int

makeLenses ''GameplayState
makeLenses ''HighScoreForm
makeLenses ''HighScoreFormState