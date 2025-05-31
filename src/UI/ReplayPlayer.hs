{-# LANGUAGE NumericUnderscores #-}

module UI.ReplayPlayer where

import Brick (App (..), BrickEvent (AppEvent, VtyEvent), EventM, customMain, get, modify, neverShowCursor, put)
import Brick.BChan (newBChan, writeBChan)
import Brick.Main (halt)
import Brick.Types (Widget)
import Brick.Widgets.Center (center)
import Control.Concurrent (MVar, forkIO, modifyMVar_, readMVar, swapMVar, threadDelay)
import Control.Concurrent.MVar (newMVar)
import Control.Monad (forever, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (runState)
import GameLogic (GameState (GameOver, NewHighScore, Playing), defaultHeight, defaultWidth, initWorld)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as V
import Linear.V2 (V2 (..))
import Logging.Replay (ReplayState (..), canExecute, runMove, stepReplayState)
import System.Random (StdGen, mkStdGen)
import UI.Types
import UI.Gameplay (drawGS, theMap)

runReplayApp :: StdGen -> InputList -> MVar Float -> IO ()
runReplayApp seed mEvList mSpeedMod = do
  chan <- newBChan 10
  _ <- forkIO $ forever $ do
    speedModifier <- readMVar mSpeedMod
    writeBChan chan Tick
    threadDelay $ ceiling (1 / abs speedModifier * (1_000_000 / 16))
  let initialVty = V.mkVty V.defaultConfig
  buildVty <- initialVty
  void $ customMain buildVty initialVty (Just chan) (replayApp mEvList mSpeedMod) (initState seed)

replayApp :: InputList -> MVar Float -> App ReplayState Tick MenuOptions
replayApp evs speedMod =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = \brickEv -> replayEventHandler brickEv evs speedMod,
      appStartEvent = replayEventHandler (AppEvent Tick) evs speedMod,
      appAttrMap = const theMap
    }

replayEventHandler :: BrickEvent n Tick -> InputList -> MVar Float -> EventM n ReplayState ()
replayEventHandler (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) _ _ = halt

replayEventHandler (VtyEvent (V.EvKey k _)) _ mSpeedMod = liftIO $ do
  case k of
    V.KChar 'n' -> modifyMVar_ mSpeedMod (pure . normalSpeed)
    V.KChar 'r' -> modifyMVar_ mSpeedMod (pure . negate)
    V.KLeft -> modifyMVar_ mSpeedMod (pure . speedDown)
    V.KRight -> modifyMVar_ mSpeedMod (pure . speedUp)
    _ -> pure ()

replayEventHandler (AppEvent Tick) evList mSpeedMod = do
  rps <- get
  speed <- liftIO $ readMVar mSpeedMod
  mapM_ (\_ -> do
    runMove speed evList
    )
    (canExecute evList rps)
  speed' <- liftIO $ readMVar mSpeedMod
  modify $ stepReplayState speed'


replayEventHandler _ _ _ = pure ()

initState :: StdGen -> ReplayState
initState seed = ReplayState {rTickNo = TickNumber 0, rGameState = start, rIndex = 0}
  where
    start = Playing $ initWorld defaultHeight defaultWidth seed

drawUI :: ReplayState -> [Widget MenuOptions]
drawUI rps = center <$> drawGS gs
  where
    gs = case rGameState rps of
      NewHighScore w -> GameOver w -- Short circuit the GameState to GameOver so it renders properly
      g -> g

replayExample :: IO ()
replayExample = do
  let evs = mkInputList evs2
  speed <- newMVar 1
  runReplayApp (mkStdGen seed2) evs speed
  -- where
  --   moves = [1, 3, 8, 10, 1, 11, 14, 1, 1]
  --   events =
  --     zipWith
  --       (GameEvent . TickNumber)
  --       (scanl' (+) 1 moves)
  --       [MoveRight, MoveDown, MoveLeft, MoveUp, MoveRight, MoveDown, MoveRight, MoveDown, MoveLeft]

speedUp, speedDown, normalSpeed :: Float -> Float
speedUp x
  | x >= 16 = x
  | otherwise = x * 2
speedDown x
  | x <= (1 / 4) = x
  | otherwise = x / 2
normalSpeed = const 1

evs2 :: [GameEvent]
evs2 =
  reverse . filter (\(GameEvent _ x) -> x `elem` [MoveUp, MoveDown, MoveLeft, MoveRight]) $
    [GameEvent 534 GameEnded,GameEvent 533 (FoodEaten (V2 10 19)),GameEvent 525 MoveUp,GameEvent 519 MoveLeft,GameEvent 513 MoveDown,GameEvent 511 (FoodEaten (V2 14 17)),GameEvent 500 MoveRight,GameEvent 499 MoveUp,GameEvent 484 (FoodEaten (V2 18 16)),GameEvent 483 MoveLeft,GameEvent 476 MoveUp,GameEvent 470 MoveRight,GameEvent 462 MoveUp,GameEvent 456 (FoodEaten (V2 19 1)),GameEvent 456 MoveLeft,GameEvent 450 MoveDown,GameEvent 435 MoveRight,GameEvent 432 MoveUp,GameEvent 424 MoveLeft,GameEvent 423 (FoodEaten (V2 12 5)),GameEvent 415 MoveDown,GameEvent 410 MoveRight,GameEvent 409 (FoodEaten (V2 7 12)),GameEvent 400 MoveUp,GameEvent 392 MoveLeft,GameEvent 384 MoveDown,GameEvent 382 (FoodEaten (V2 13 11)),GameEvent 381 MoveRight,GameEvent 374 MoveUp,GameEvent 364 MoveRight,GameEvent 360 (FoodEaten (V2 2 8)),GameEvent 360 MoveDown,GameEvent 356 MoveLeft,GameEvent 349 MoveDown,GameEvent 346 MoveRight,GameEvent 338 MoveUp,GameEvent 328 MoveLeft,GameEvent 321 MoveDown,GameEvent 315 MoveRight,GameEvent 313 (FoodEaten (V2 7 12)),GameEvent 312 MoveUp,GameEvent 306 MoveLeft,GameEvent 297 MoveUp,GameEvent 291 MoveRight,GameEvent 290 (FoodEaten (V2 7 3)),GameEvent 287 MoveDown,GameEvent 278 MoveLeft,GameEvent 273 MoveDown,GameEvent 271 (FoodEaten (V2 14 11)),GameEvent 267 MoveRight,GameEvent 262 MoveUp,GameEvent 257 MoveLeft,GameEvent 256 (FoodEaten (V2 15 7)),GameEvent 251 MoveDown,GameEvent 248 MoveRight,GameEvent 241 (FoodEaten (V2 12 5)),GameEvent 241 MoveUp,GameEvent 235 MoveRight,GameEvent 231 MoveDown,GameEvent 229 (FoodEaten (V2 8 9)),GameEvent 226 MoveLeft,GameEvent 222 MoveDown,GameEvent 218 MoveRight,GameEvent 212 MoveUp,GameEvent 208 MoveLeft,GameEvent 207 (FoodEaten (V2 11 8)),GameEvent 201 MoveDown,GameEvent 197 MoveRight,GameEvent 192 MoveDown,GameEvent 190 (FoodEaten (V2 9 19)),GameEvent 184 MoveLeft,GameEvent 169 MoveUp,GameEvent 164 (FoodEaten (V2 10 4)),GameEvent 163 MoveRight,GameEvent 156 MoveDown,GameEvent 149 (FoodEaten (V2 16 11)),GameEvent 148 MoveLeft,GameEvent 142 MoveDown,GameEvent 140 MoveRight,GameEvent 132 MoveUp,GameEvent 122 MoveRight,GameEvent 118 MoveDown,GameEvent 116 (FoodEaten (V2 7 13)),GameEvent 115 MoveLeft,GameEvent 105 MoveUp,GameEvent 94 MoveLeft,GameEvent 93 (FoodEaten (V2 19 4)),GameEvent 85 MoveDown,GameEvent 81 MoveRight,GameEvent 76 MoveDown,GameEvent 73 (FoodEaten (V2 12 17)),GameEvent 72 MoveRight,GameEvent 66 MoveUp,GameEvent 59 (FoodEaten (V2 18 11)),GameEvent 58 MoveLeft,GameEvent 48 MoveUp,GameEvent 31 (FoodEaten (V2 2 1)),GameEvent 30 MoveRight,GameEvent 26 MoveDown,GameEvent 12 (FoodEaten (V2 15 5)),GameEvent 11 MoveLeft,GameEvent 6 MoveDown,GameEvent 0 MoveRight]

seed2 :: SeedType
seed2 = 9157570410566699885



