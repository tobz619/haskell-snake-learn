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
import Control.Monad (when)

type Seed = StdGen

type Replay = EventList -> GameState

data ReplayState = ReplayState
  { rGameState :: !GameState,
    rTickNo :: !TickNumber,
    rCheckPoint :: CheckPointMap,
    rGameStateVec :: Either RewindBuffer [(Int, GameState)],
    rEvIndex :: !Int,
    rRewindIndex :: !Int
  }


-- runReplayG :: Replay
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
  runMove evs

canExecute :: InputList -> ReplayState -> Maybe GameEvent
canExecute evList (ReplayState _ t1 _ _ ix _) =
  evList V.!? ix >>= \g@(GameEvent t0 _) -> if t0 == t1
                      then pure g
                      else Nothing

addCheckPoint :: MonadState ReplayState m => InputList -> m ()
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


handleSpeed :: MonadState ReplayState m => Float -> InputList -> m ()
handleSpeed s evList
  | s > 0 = do
             rps <- get
             let rws = rGameStateVec rps
             either (stepRewindF evList) (\_ -> runMove evList) rws

  | otherwise = do
            rps <- get
            let rws = either id mkRewindBuffer (rGameStateVec rps)
            modify (\r -> r {rGameStateVec = Left rws})
            stepRewindR rws 


runMove :: (MonadState ReplayState m) => InputList -> m ()
runMove evList = do
      rps <- get
      mapM_
        (\(GameEvent _ kev) -> do
          modify $ \r -> r {rGameState = executeMove kev (rGameState rps) pairs}
          modify $ \r -> r {rEvIndex = rEvIndex rps + 1}
          runMove evList -- Make sure all events are executed at that moment!
        )
        (canExecute evList rps)
      modify stepReplayState

      
      
      
      



-- stepRewindF :: MonadState ReplayState m => InputList -> ReplayState -> RewindBuffer -> m ()
stepRewindF :: MonadState ReplayState m => InputList -> RewindBuffer -> m ()
stepRewindF evList arr = do
  ix <- gets rRewindIndex
  if ix < 0
    then do 
    modify (\rps -> rps {
      rGameStateVec = pure mempty,
      rEvIndex = fst $ arr V.! 0,
      rTickNo = rTickNo rps - 1, 
      rRewindIndex = 0
      }
      )
    runMove evList
  else do
      modify (\rps -> rps {
        rGameState = snd $ arr V.! rRewindIndex rps,
        rEvIndex =  fst $ arr V.! rRewindIndex rps
        }
        )
      modify decreaseRewindIndex

      

stepRewindR :: MonadState ReplayState m => RewindBuffer -> m ()
stepRewindR arr = do
  ix <- gets rRewindIndex
  if ix >= endOfArray
    then pure ()
    else do 
      modify (increaseRewindIndex arr)
      modify (\rps -> rps {
        rGameState = snd $ arr V.! rRewindIndex rps,
        rEvIndex = fst $ arr V.! rRewindIndex rps
        }
        )
    where endOfArray = V.length arr - 1

increaseRewindIndex :: RewindBuffer -> ReplayState -> ReplayState
increaseRewindIndex arr rps@(ReplayState {..})
  | rRewindIndex >= V.length arr = rps {rRewindIndex = V.length arr}
  | otherwise = rps {rRewindIndex = rRewindIndex + 1}

decreaseRewindIndex :: ReplayState -> ReplayState
decreaseRewindIndex rps@(ReplayState{..})
  | rRewindIndex < 0 = rps
  | otherwise = rps {rRewindIndex = rRewindIndex - 1}

stepReplayState :: ReplayState -> ReplayState
stepReplayState (ReplayState {..}) =
  ReplayState
    (stepGameState rGameState)
    (rTickNo + 1)
    rCheckPoint
    (((rEvIndex, rGameState) :) <$> rGameStateVec)
    rEvIndex
    rRewindIndex


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
