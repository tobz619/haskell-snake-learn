{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Logging.Replay where

import Control.Monad.State (MonadState, State, execState)
import qualified Data.Map.Strict as Map
import GameLogic (Direction (..), GameState (..), chDir, stepGameState)
import System.Random (StdGen)
import UI.Types
import qualified Data.Vector.Strict as V
import Brick
import Control.Monad (when, unless)

type Seed = StdGen

type Replay = EventList -> GameState

data ReplayState = ReplayState
  { rGameState :: !GameState,
    rTickNo :: !TickNumber,
    rCheckPoint :: CheckPointMap,
    rGameStateVec :: Either RewindBuffer [RewindType],
    rEvIndex :: !EvNumber,
    rRewindIndex :: !Int
  }

-- | Runs a silent replay that will return the final GameState of the Game.
runReplayG :: EventList -> ReplayState -> GameState
runReplayG es rps = case execState (runReplay (mkInputList es)) rps of
  newS
   | isGameOver (rGameState newS) -> rGameState newS
   | otherwise -> runReplayG es (stepReplayState newS)
   -- ^ Keep stepping the state forward until the game hits the end state.

isGameOver :: GameState -> Bool
isGameOver (GameOver _) = True
isGameOver (NewHighScore _) = True
isGameOver _ = False

-- runReplay :: Seed -> EventList -> ReplayState
runReplay ::  InputList -> State ReplayState ()
runReplay evs = do
  modify stepReplayState
  runMoveM evs

canExecute :: InputList -> ReplayState -> Maybe GameEvent
canExecute evList (ReplayState _ t1 _ _ (EvNumber ix) _) =
  evList V.!? ix >>= \g@(GameEvent t0 _) -> if t0 == t1
                      then pure g
                      else Nothing

addCheckPoint :: MonadState ReplayState m => InputList -> m ()
addCheckPoint evList = do
  rps <- get
  gs <- gets rGameState
  tn <- gets rTickNo
  cpMap <- gets rCheckPoint
  mapM_ (\g -> when (isCheckPointEvent g) $ modify (\r -> r {rCheckPoint = Map.insert tn gs cpMap}))
        (canExecute evList rps)

    where isCheckPointEvent (GameEvent _ k ) = case k of
              FoodEaten _ -> True
              GameEnded -> True
              GameStarted -> True
              _ -> False


handleSpeed :: MonadState ReplayState m => Float -> InputList -> m ()
handleSpeed s evList
  | s > 0 = do
             rws <- gets rGameStateVec
             either (stepRewindF evList) (\_ -> runMoveM evList >> addCheckPoint evList) rws
             gs <- gets rGameState
             unless (isGameOver gs) $ modify stepReplayState

  | otherwise = do
            gs <- gets rGameState
            unless (isGameOver gs) $ modify stepReplayState
            rps <- get
            let rws = either id mkRewindBuffer (rGameStateVec rps)
            modify (\r -> r {rGameStateVec = Left rws})
            stepRewindR rws

-- | Runs the move found in the pairs library
runMove :: InputList -> ReplayState -> Maybe ReplayState
runMove evList rps =

        (\(GameEvent _ kev) ->
          rps {
            rGameState = executeMove kev (rGameState rps) pairs,
            rEvIndex = rEvIndex rps + 1
          }
        ) <$>
        canExecute evList rps


runMoveM :: MonadState ReplayState m => InputList -> m ()
runMoveM evList = do 
  rps <- get 
  mapM_ put (runMove evList rps)

-- stepRewindF :: MonadState ReplayState m =>  RewindBuffer -> m ()
stepRewindF evList arr = do
  ix <- gets rRewindIndex
  if ix <= 0
    then do
    modify (\rps -> rps {
      rGameStateVec = pure . V.toList $ arr,
      rEvIndex = rwEv $ arr V.! ix,
      rTickNo = rwTick $ arr V.! ix
      }
      )
    runMoveM evList
  else do
    modify (\rps -> rps {
      rGameState = rwGS $ arr V.! ix,
      rEvIndex =  rwEv $ arr V.! ix,
      rTickNo = rwTick $ arr V.! ix
      }
      )
    modify decreaseRewindIndex



stepRewindR :: MonadState ReplayState m => RewindBuffer -> m ()
stepRewindR arr = do
  ix <- gets rRewindIndex
  modify $ \rps -> rps {
        rGameState = rwGS $ arr V.! ix,
        rEvIndex = rwEv $ arr V.! ix,
        rTickNo = rwTick $ arr V.! ix
        }
  unless (ix >= endOfArray) $ do
     modify $ \rps -> rps {
        rTickNo = rwTick $ arr V.! ix
        }

  modify (increaseRewindIndex arr)

    where endOfArray = V.length arr - 1

increaseRewindIndex :: RewindBuffer -> ReplayState -> ReplayState
increaseRewindIndex arr rps@(ReplayState {..})
  | rRewindIndex >= V.length arr - 1 = rps {rRewindIndex = V.length arr - 1 }
  | otherwise = rps { rRewindIndex = rRewindIndex + 1 }

decreaseRewindIndex :: ReplayState -> ReplayState
decreaseRewindIndex rps@(ReplayState{..})
  | rRewindIndex < 0 = rps {rRewindIndex = 0}
  | otherwise = rps { rRewindIndex = rRewindIndex - 1 }

stepReplayState :: ReplayState -> ReplayState
stepReplayState (ReplayState {..}) =
  ReplayState {
    rGameState = stepGameState rGameState,
    rTickNo = rTickNo + 1,
    rCheckPoint = rCheckPoint,
    rGameStateVec = (RewindType rTickNo rEvIndex rGameState :) <$> rGameStateVec,
    rEvIndex = rEvIndex,
    rRewindIndex = rRewindIndex
    }


executeMove :: KeyEvent -> GameState -> Map.Map KeyEvent (GameState -> GameState) -> GameState
executeMove ev gs mp =
  maybe gs ($ gs) (Map.lookup ev mp)

pairs :: Map.Map KeyEvent (GameState -> GameState)
pairs =
  Map.fromList
    [ (MoveUp, chDir U),
      (MoveDown, chDir D),
      (MoveLeft, chDir L),
      (MoveRight, chDir R),
      (GameEnded, GameOver . getWorld ) -- Should hopefully only ever poll @GameState@s with a valid world 
    ]
