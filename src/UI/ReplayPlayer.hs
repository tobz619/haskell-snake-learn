{-# LANGUAGE NumericUnderscores #-}

module UI.ReplayPlayer where

import Brick (App (..), BrickEvent (AppEvent, VtyEvent), EventM, customMain, get, modify, neverShowCursor, put)
import Brick.BChan (newBChan, writeBChan)
import Brick.Main (halt)
import Brick.Types (Widget)
import Control.Concurrent (MVar, forkIO, readMVar, threadDelay, swapMVar)
import Control.Monad (forever, void, when)
import Control.Monad.IO.Class (liftIO)
import GameLogic (GameState (Playing), defaultHeight, defaultWidth, initWorld)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as V
import Logging.Logger (EventList, GameEvent (GameEvent))
import Logging.Replay (ReplayState (..), stepReplayState, canExecute, runMove)
import System.Random (StdGen, mkStdGen)
import UI.Gameplay (MenuOptions, Tick (Tick), drawGS, theMap)
import Brick.Widgets.Center (center)
import Control.Concurrent.MVar (newMVar)
import Data.List (scanl')
import UI.Keybinds (KeyEvent(..))
import Control.Monad.State.Strict (runState)

runReplayApp :: StdGen -> MVar EventList -> IO ()
runReplayApp seed mEvList = do
  chan <- newBChan 10
  _ <- forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 100_000
  let initialVty = V.mkVty V.defaultConfig
  buildVty <- initialVty
  void $ customMain buildVty initialVty (Just chan) (replayApp mEvList) (initState seed)

replayApp :: MVar EventList -> App ReplayState Tick MenuOptions
replayApp evs =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = flip replayEventHandler evs,
      appStartEvent = replayEventHandler (AppEvent Tick) evs,
      appAttrMap = const theMap
    }

replayEventHandler :: BrickEvent n Tick -> MVar EventList -> EventM n ReplayState ()
replayEventHandler (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) _ = halt
replayEventHandler (AppEvent Tick) mVarEv = do
  modify stepReplayState
  rps <- get
  evlist <- liftIO $ readMVar mVarEv
  when (canExecute evlist rps) $ do
    let (newEvs, newS) = runState (runMove evlist) rps
    put newS
    void . liftIO $ swapMVar mVarEv newEvs

replayEventHandler _ _ = return ()


initState :: StdGen -> ReplayState
initState seed = ReplayState {tickNo = 0, gameState = start}
  where
    start = Playing $ initWorld defaultHeight defaultWidth seed

drawUI :: ReplayState -> [Widget MenuOptions]
drawUI rps = center <$> drawGS gs
  where
    gs = gameState rps



replayExample :: IO ()
replayExample = do
  evs <- newMVar events
  runReplayApp (mkStdGen 4) evs
  where
    moves = [1,3,8,10,1,11,14,1,1]
    events = zipWith GameEvent
      (scanl' (+) 1 moves)
      [MoveRight, MoveDown, MoveLeft, MoveUp, MoveRight, MoveDown, MoveRight, MoveDown, MoveLeft] ++
      zipWith GameEvent
        (drop 1 $ iterate (+ 3) (sum moves))
        (cycle [MoveUp, MoveLeft, MoveDown, MoveRight])
