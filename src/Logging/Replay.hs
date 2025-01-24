{-# LANGUAGE FlexibleContexts #-}
module Logging.Replay where

import Brick (get, put)
import Control.Monad.State (execState, MonadState)
import qualified Data.Map as Map
import GameLogic (Direction (..), GameState (Playing), chDir, initWorld, stepGameState)
import Logging.Logger (EventList, GameEvent (..))
import System.Random (StdGen)
import UI.Keybinds (KeyEvent (..))

type Seed = StdGen

type Replay = Seed -> EventList -> ReplayState -> GameState

data PlayState = Forward Int | Reverse Int

data ReplayState = ReplayState
  { gameState :: GameState,
    tickNo :: Int
  }

runReplay :: StdGen -> EventList -> ReplayState
runReplay seed evs =
  execState
    (runMove evs)
    ( ReplayState
        (Playing $ initWorld 20 20 seed)
        0
    )

canExecute :: EventList -> ReplayState -> Bool
canExecute [] _ = False
canExecute ((GameEvent t0 _):_) (ReplayState _ t1) = t0 == t1

runMove :: MonadState ReplayState m => EventList -> m EventList
runMove [] = return []
runMove evList@(GameEvent _ kev: _) = do
  rps <- get
  if canExecute evList rps
    then do
      put $ rps {gameState = executeMove kev (gameState rps)}
      return $ drop 1 evList
    else do
      put . stepReplayState =<< get
      return evList
      

stepReplayState :: ReplayState -> ReplayState
stepReplayState (ReplayState gs tick) =
  ReplayState (stepGameState gs) (tick + 1)

executeMove :: KeyEvent -> GameState -> GameState
executeMove ev gs =
  maybe gs ($ gs) (Map.lookup ev pairs)

pairs :: Map.Map KeyEvent (GameState -> GameState)
pairs =
  Map.fromList
    [ (MoveUp, chDir U),
      (MoveDown, chDir D),
      (MoveLeft, chDir L),
      (MoveRight, chDir R)
    ]
