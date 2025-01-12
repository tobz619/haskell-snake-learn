module Logging.Replay where

import Brick (get, gets, modify, put)
import Control.Monad.State (State, evalState)
import qualified Data.Map as Map
import GameLogic (Direction (..), GameState (Playing), chDir, initWorld, stepGameState)
import Logging.Logger (EventList, GameEvent (..))
import System.Random (StdGen)
import UI.Keybinds (KeyEvent (..))

type Seed = StdGen

type Replay = Seed -> EventList -> ReplayState -> GameState

data ReplayState = ReplayState
  { gameState :: GameState,
    tickNo :: Int
  }

runReplay :: StdGen -> EventList -> GameState
runReplay seed evs =
  evalState
    (runMoves evs)
    ( ReplayState
        (Playing $ initWorld 20 20 seed)
        0
    )

runMoves :: EventList -> State ReplayState GameState
runMoves [] = gets gameState
runMoves (GameEvent t0 kev : evs) = do
  a@(ReplayState _ t1) <- get
  if t0 /= t1
    then do
      put . stepReplayState =<< get
      runMoves evs
    else do
      modify $ \rps -> rps {gameState = executeMove kev (gameState a)}
      modify stepReplayState
      runMoves evs

stepReplayState :: ReplayState -> ReplayState
stepReplayState (ReplayState gs tick) =
  ReplayState (stepGameState gs) (tick + 1)

executeMove :: KeyEvent -> GameState -> GameState
executeMove ev gs =
  maybe gs (\f -> f gs) (Map.lookup ev pairs)

pairs :: Map.Map KeyEvent (GameState -> GameState)
pairs =
  Map.fromList
    [ (MoveUp, chDir U),
      (MoveDown, chDir D),
      (MoveLeft, chDir L),
      (MoveRight, chDir R)
    ]
