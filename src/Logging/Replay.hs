{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Logging.Replay where

import Brick (get, put)
import Control.Monad.State (MonadState, runState, modify, State, execState)
import qualified Data.Map as Map
import GameLogic (Direction (..), GameState (..), chDir, stepGameState, reverseGameState, stepReverseGameState)
import System.Random (StdGen)
import UI.Types
import qualified Data.Vector.Strict as V
import Brick
import Control.Monad (when)

type Seed = StdGen

type Replay = EventList -> GameState

data PlayState = Forward Int | Reverse Int

data ReplayState = ReplayState
  { rGameState :: !GameState,
    rTickNo :: !TickNumber,
    rCheckPoint :: CheckPointMap,
    rIndex :: !Int
  }

-- runReplayG :: Replay
runReplayG :: EventList -> ReplayState -> GameState
runReplayG es rps = case execState (runReplay (mkInputList es)) rps of
  newS
   | isGameOver (rGameState newS) -> rGameState newS
   | otherwise -> runReplayG es (stepReplayState 1 newS)
   -- ^ Keep stepping the state forward until the game hits the end state.

isGameOver :: GameState -> Bool
isGameOver (GameOver _) = True
isGameOver (NewHighScore _) = True
isGameOver _ = False

-- runReplay :: Seed -> EventList -> ReplayState
runReplay ::  InputList -> State ReplayState ()
runReplay evs = do
  modify (stepReplayState 1)
  runMove 1 evs

canExecute :: InputList -> ReplayState -> Maybe GameEvent
canExecute evList (ReplayState _ t1 _ ix)
  | ix < 0 = Nothing
  | ix >= V.length evList = Nothing
  | otherwise = let g@(GameEvent t0 _) = evList V.! ix
                 in if t0 == t1
                      then pure g
                      else Nothing

addCheckPoint evList = do
  rps <- get
  gs <- gets rGameState
  tn <- gets rTickNo
  cpMap <- gets rCheckPoint
  mapM_ (\g -> when (isFoodEvent g) $ modify (\r -> r {rCheckPoint = Map.insert tn gs cpMap}))
        (canExecute evList rps)

    where isFoodEvent (GameEvent _ k ) = case k of
              FoodEaten _ -> True
              _ -> False



runMove :: (MonadState ReplayState m) => Float -> InputList -> m ()
runMove s evList
  | s > 0 = do
      rps <- get
      maybe
        (do
          pure ()
          )
        (\(GameEvent _ kev) -> do
          put $ rps {rGameState = executeMove kev (rGameState rps) pairs}
          modify (moveIndex s evList)
          runMove s evList -- Make sure all events are executed at that moment!
        )
        (canExecute evList rps)

  | otherwise = do
      rps <- get
      maybe ( do
        pure ()
        )
        (\(GameEvent _ kev) -> do
          put $ rps {rGameState = executeMove kev (rGameState rps) reversePairs}
          modify (moveIndex s evList)
          runMove s evList
        )
        (canExecute evList rps)


moveIndex :: Float -> InputList -> ReplayState -> ReplayState
moveIndex s events rps@(ReplayState {..})
  | s < 0 && rIndex > 0 = rps {rIndex = rIndex - 1}
  | rIndex >= V.length events = rps {rIndex = V.length events}
  | s > 0 = rps {rIndex = rIndex + 1}
  | otherwise = rps

stepReplayState :: Float -> ReplayState -> ReplayState
stepReplayState s (ReplayState {..})
  | s > 0 = ReplayState (stepGameState rGameState) (rTickNo + 1) rCheckPoint rIndex
  | otherwise = ReplayState (stepReverseGameState rGameState) (rTickNo - 1) rCheckPoint rIndex



executeMove :: KeyEvent -> GameState -> Map.Map KeyEvent (GameState -> GameState) -> GameState
executeMove ev gs mp =
  maybe gs ($ gs) (Map.lookup ev mp)

pairs, reversePairs :: Map.Map KeyEvent (GameState -> GameState)
pairs =
  Map.fromList
    [ (MoveUp, chDir U),
      (MoveDown, chDir D),
      (MoveLeft, chDir L),
      (MoveRight, chDir R),
      (GameEnded, GameOver . getWorld ) -- Should hopefully only ever poll @GameState@s with a valid world 
    ]

reversePairs =
  Map.fromList
    [ (MoveUp, chDir U),
      (MoveDown, chDir D),
      (MoveLeft, chDir L),
      (MoveRight, chDir R),
      (GameEnded, Playing . getWorld ) -- Should hopefully only ever poll @GameState@s with a valid world 
    ]
