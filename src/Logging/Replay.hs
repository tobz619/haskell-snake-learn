{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module Logging.Replay where

import Brick (get, put)
import Control.Monad.State (MonadState, runState, modify, State)
import qualified Data.Map as Map
import GameLogic (Direction (..), GameState (GameOver, getWorld, NewHighScore), chDir, stepGameState)
import Logging.Logger (EventList, GameEvent (..), TickNumber (TickNumber))
import System.Random (StdGen)
import UI.Keybinds (KeyEvent (..))

type Seed = StdGen

type Replay = EventList -> GameState

data PlayState = Forward Int | Reverse Int

data ReplayState = ReplayState
  { rGameState :: GameState,
    rTickNo :: TickNumber
  }

-- runReplayG :: Replay
runReplayG :: EventList -> ReplayState -> GameState
runReplayG es rps = case runState (runReplay es) rps of
  ([], newS)
   | isGameOver (rGameState newS) -> rGameState newS
   | otherwise -> runReplayG [] (stepReplayState newS) 
   -- ^ Keep stepping the state forward until the game hits the end state.
  (newEvs, newS) -> runReplayG newEvs newS

isGameOver :: GameState -> Bool
isGameOver (GameOver _) = True
isGameOver (NewHighScore _) = True
isGameOver _ = False

-- runReplay :: Seed -> EventList -> ReplayState
runReplay ::  EventList -> State ReplayState EventList
runReplay [] = pure []
runReplay evs = do
  modify stepReplayState
  rps <- get
  if canExecute evs rps
    then do
      let (newEvs, newS) = runState (runMove evs) rps
      put newS
      -- put (traceShowWith (getWorld . gameState) newS)
      pure newEvs
    else
      pure evs

canExecute :: EventList -> ReplayState -> Bool
canExecute [] _ = False
canExecute ((GameEvent t0 _) : _) (ReplayState _ t1) = t0 == t1

runMove :: (MonadState ReplayState m) => EventList -> m EventList
runMove [] = return []
runMove evList@(GameEvent _ kev : _) = do
  rps <- get
  if canExecute evList rps
    then do
      put $ rps {rGameState = executeMove kev (rGameState rps)}
      return $ drop 1 evList
    else do
      put . stepReplayState =<< get
      return evList

stepReplayState :: ReplayState -> ReplayState
stepReplayState (ReplayState !gs (TickNumber !tick)) =
  ReplayState (stepGameState gs) (TickNumber $ tick + 1)

executeMove :: KeyEvent -> GameState -> GameState
executeMove ev gs =
  maybe gs ($ gs) (Map.lookup ev pairs)

pairs :: Map.Map KeyEvent (GameState -> GameState)
pairs =
  Map.fromList
    [ (MoveUp, chDir U),
      (MoveDown, chDir D),
      (MoveLeft, chDir L),
      (MoveRight, chDir R),
      (GameEnded, GameOver . getWorld ) -- Should hopefully only ever poll @GameState@s with a valid world 
    ]

