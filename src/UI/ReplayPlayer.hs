{-# LANGUAGE NumericUnderscores #-}

module UI.ReplayPlayer where

import Brick (App (..), BrickEvent (AppEvent, VtyEvent), EventM, customMain, get, modify, neverShowCursor, put)
import Brick.BChan (newBChan, writeBChan)
import Brick.Main (halt)
import Brick.Types (Widget)
import Control.Concurrent (MVar, forkIO, readMVar, threadDelay, swapMVar, takeMVar, putMVar)
import Control.Monad (forever, void, when)
import Control.Monad.IO.Class (liftIO)
import GameLogic (GameState (Playing, NewHighScore, GameOver), defaultHeight, defaultWidth, initWorld)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as V
import Logging.Logger (EventList, GameEvent (GameEvent), TickNumber (TickNumber))
import Logging.Replay (ReplayState (..), stepReplayState, canExecute, runMove)
import System.Random (StdGen, mkStdGen)
import UI.Gameplay (MenuOptions, Tick (Tick), drawGS, theMap)
import Brick.Widgets.Center (center)
import Control.Concurrent.MVar (newMVar)
import Data.List (scanl')
import UI.Keybinds (KeyEvent(..))
import Control.Monad.State.Strict (runState)

runReplayApp :: StdGen -> MVar EventList -> MVar Float -> IO ()
runReplayApp seed mEvList mSpeedMod = do
  chan <- newBChan 10
  _ <- forkIO $ forever $ do
    speedModifier <- readMVar mSpeedMod
    writeBChan chan Tick
    threadDelay $ ceiling (1 / speedModifier * 100_000)
  let initialVty = V.mkVty V.defaultConfig
  buildVty <- initialVty
  void $ customMain buildVty initialVty (Just chan) (replayApp mEvList mSpeedMod) (initState seed)

replayApp :: MVar EventList -> MVar Float -> App ReplayState Tick MenuOptions
replayApp evs speedMod =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = \brickEv -> replayEventHandler brickEv evs speedMod,
      appStartEvent = replayEventHandler (AppEvent Tick) evs speedMod,
      appAttrMap = const theMap
    }

replayEventHandler :: BrickEvent n Tick -> MVar EventList -> MVar Float -> EventM n ReplayState ()
replayEventHandler (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) _ _ = halt
replayEventHandler (AppEvent Tick) mVarEv _ = do
  modify stepReplayState
  rps <- get
  evlist <- liftIO $ readMVar mVarEv
  when (canExecute evlist rps) $ do
    let (newEvs, newS) = runState (runMove evlist) rps
    put newS
    void . liftIO $ swapMVar mVarEv newEvs

replayEventHandler (VtyEvent (V.EvKey (V.KChar 'r') [])) _ mSpeedMod =  liftIO $ do
  s <- takeMVar mSpeedMod
  putMVar mSpeedMod (normalSpeed s)

replayEventHandler (VtyEvent (V.EvKey V.KLeft [])) _ mSpeedMod =  liftIO $ do
  s <- takeMVar mSpeedMod
  putMVar mSpeedMod (speedDown s)
  

replayEventHandler (VtyEvent (V.EvKey V.KRight [])) _ mSpeedMod =  liftIO $ do
  s <- takeMVar mSpeedMod
  putMVar mSpeedMod (speedUp s)

replayEventHandler _ _ _ = return ()


initState :: StdGen -> ReplayState
initState seed = ReplayState {tickNo = TickNumber 0, gameState = start}
  where
    start = Playing $ initWorld defaultHeight defaultWidth seed

drawUI :: ReplayState -> [Widget MenuOptions]
drawUI rps = center <$> drawGS gs
  where
    gs = case gameState rps of
          NewHighScore w -> GameOver w -- Short circuit the GameState to GameOver so it renders properly
          g -> g



replayExample :: IO ()
replayExample = do
  evs <- newMVar events
  speed <- newMVar 1
  runReplayApp (mkStdGen 4) evs speed
  where
    moves = [1,3,8,10,1,11,14,1,1]
    events = zipWith (GameEvent . TickNumber)
      (scanl' (+) 1 moves)
      [MoveRight, MoveDown, MoveLeft, MoveUp, MoveRight, MoveDown, MoveRight, MoveDown, MoveLeft]
      ++
      zipWith (GameEvent . TickNumber)
        (drop 1 $ iterate (+ 3) (sum moves))
        (cycle [MoveUp, MoveLeft, MoveDown, MoveRight])


speedUp, speedDown, normalSpeed :: Float -> Float
speedUp x
  | x >= 16 = x
  | otherwise = x * 2

speedDown x
  | x <= (1/4) = x
  | otherwise = x / 2

normalSpeed = const 1