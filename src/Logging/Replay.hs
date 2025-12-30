{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Logging.Replay where

import Control.Monad.State (MonadState)
import qualified Data.Map.Strict as Map
import GameLogic (Direction (..), GameState (..), chDir, stepGameState, initWorld, defaultHeight, defaultWidth)
import System.Random (StdGen, mkStdGen)
import UI.Types
import qualified Data.Vector.Strict as V
import Brick
import Control.Monad (when, unless)
import Data.Maybe (fromMaybe)

type Seed = StdGen

type Replay = EventList -> GameState

data ReplayState = ReplayState
  { rGameState :: !GameState,
    rTickNo :: !TickNumber,
    rCheckPoint :: Checkpoints,
    rGameStateVec :: Either RewindBuffer [RewindType],
    rEvIndex :: !EvNumber,
    rRewindIndex :: !Int
  }

type Checkpoints = V.Vector ReplayState


-- | Runs a silent replay that will return the final GameState of the Game.
runReplayG :: EventList -> ReplayState -> GameState
runReplayG es = rGameState . V.last .  generateAllStates es
   -- ^ Keep stepping the state forward until the game hits the end state.

generateCheckPoints :: EventList -> ReplayState -> V.Vector ReplayState
generateCheckPoints es = takeEvery 20 . generateAllStates es

generateAllStates :: EventList -> ReplayState -> V.Vector ReplayState
generateAllStates [] = V.singleton
generateAllStates es = V.unfoldr (runReplay (mkInputList es))


takeEvery :: Int -> V.Vector a -> V.Vector a
takeEvery n xs
  | V.null xs = xs
  | otherwise = one V.++ rest
  where one = V.take 1 xs
        rest = takeEvery n (V.drop n xs)


isGameOver :: GameState -> Bool
isGameOver (GameOver _) = True
isGameOver (NewHighScore _) = True
isGameOver (Paused _) = True
isGameOver _ = False


runReplay :: InputList -> ReplayState -> Maybe (ReplayState, ReplayState)
runReplay evs rps = let nextState = fromMaybe rps (runMove evs rps)
                        ret = stepReplayState nextState
                     in if isGameOver (rGameState ret)
                          then Nothing
                          else Just (ret, ret)

canExecute :: InputList -> ReplayState -> Maybe GameEvent
canExecute evList (ReplayState _ t1 _ _ (EvNumber ix) _) =
  evList V.!? ix >>= \g@(GameEvent t0 _) -> if t0 == t1
                      then pure g
                      else Nothing

addCheckPoint :: MonadState ReplayState m => InputList -> m ()
addCheckPoint inList = do
  let lLength = length inList
  tn <- gets rTickNo
  when (tn `mod` (fromIntegral lLength `mod` 10) == 0) -- Ten checkpoints per replay
    (modify $ \r -> r {rCheckPoint = r `V.cons` rCheckPoint r})


handleSpeed :: MonadState ReplayState m => Double -> InputList -> m ()
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

stepRewindF :: MonadState ReplayState m => InputList -> RewindBuffer -> m ()
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

initState :: StdGen -> ReplayState
initState seed =
  ReplayState
    { rTickNo = TickNumber 0,
      rGameState = start,
      rGameStateVec = Right [newRwType start],
      rCheckPoint = V.empty,
      rEvIndex = 0,
      rRewindIndex = 0
    }
  where
    start = Playing $ initWorld defaultHeight defaultWidth seed
    newRwType = RewindType (TickNumber 0) (EvNumber 0)

testReplayG = 
  let evs = mkEvs [GameEvent {gEvTick = 56, gEvEvent = GameEnded},GameEvent {gEvTick = 51, gEvEvent = MoveRight},GameEvent {gEvTick = 47, gEvEvent = MoveDown},GameEvent {gEvTick = 44, gEvEvent = MoveLeft},GameEvent {gEvTick = 39, gEvEvent = MoveUp},GameEvent {gEvTick = 35, gEvEvent = MoveRight},GameEvent {gEvTick = 26, gEvEvent = MoveDown},GameEvent {gEvTick = 14, gEvEvent = MoveRight},GameEvent {gEvTick = 8, gEvEvent = MoveUp},GameEvent {gEvTick = 0, gEvEvent = MoveLeft},GameEvent {gEvTick = 0, gEvEvent = GameStarted}]
      seed = -8957748602158299037
   in runReplayG evs (initState (mkStdGen seed))