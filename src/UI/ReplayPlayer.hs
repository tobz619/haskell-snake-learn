{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}

module UI.ReplayPlayer where

import Brick (App (..), BrickEvent (AppEvent, VtyEvent), EventM, customMain, get, modify, neverShowCursor, put)
import Brick.BChan (newBChan, writeBChan)
import Brick.Main (halt)
import Brick.Types (Widget)
import Control.Concurrent (MVar, forkIO, newMVar, putMVar, readMVar, takeMVar, threadDelay, swapMVar)
import Control.Monad (forever, void, when)
import Control.Monad.IO.Class (liftIO)
import GameLogic (GameState (Playing), defaultHeight, defaultWidth, initWorld)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as V
import Logging.Logger (EventList, GameEvent (GameEvent))
import Logging.Replay (ReplayState (..), executeMove, stepReplayState)
import System.Random (StdGen, mkStdGen)
import UI.Gameplay (MenuOptions, Tick (Tick), drawGS, theMap)
import UI.Keybinds (KeyEvent (..))

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
    newEvs <- makeMove evlist rps
    _ <- liftIO $ swapMVar mVarEv newEvs
    return ()

replayEventHandler _ _ = return ()

canExecute :: EventList -> ReplayState -> Bool
canExecute [] _ = False
canExecute ((GameEvent t0 _):_) (ReplayState _ t1) = t0 == t1

makeMove :: EventList -> ReplayState -> EventM n ReplayState EventList
makeMove [] _ = return []
makeMove (GameEvent _ ev : res) rps =
  do
    put $ rps {gameState = executeMove ev (gameState rps)}
    return res

initState :: StdGen -> ReplayState
initState seed = ReplayState {tickNo = 0, gameState = start}
  where
    start = Playing $ initWorld defaultHeight defaultWidth seed

drawUI :: ReplayState -> [Widget MenuOptions]
drawUI rps = drawGS gs
  where
    gs = gameState rps

replayExample :: IO ()
replayExample = do
  evs <- newMVar events
  runReplayApp (mkStdGen 4) evs
  where
    events = zipWith GameEvent (iterate (+ 3) 3) (cycle [MoveLeft, MoveDown, MoveRight, MoveUp])