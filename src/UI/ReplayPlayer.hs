{-# LANGUAGE NumericUnderscores #-}
module ReplayPlayer where

import Brick.Types (Widget)
import Logging.Replay (ReplayState (..), stepReplayState, executeMove)
import UI.Gameplay (MenuOptions, drawGS, theMap, Tick (Tick))
import Brick.BChan (newBChan, writeBChan)
import Control.Concurrent (threadDelay, forkIO)
import qualified Graphics.Vty.CrossPlatform as V
import qualified Graphics.Vty.Config as V
import Control.Monad (forever, void, when)
import Brick (customMain, App (..), neverShowCursor, BrickEvent (AppEvent), get, modify, EventM, put)
import GameLogic (initWorld, defaultHeight, defaultWidth, GameState (Playing))
import System.Random (StdGen)
import Logging.Logger (EventList, GameEvent (GameEvent))

replay seed = do
  chan <- newBChan 10
  _ <- forkIO $ forever $ do
      writeBChan chan Tick
      threadDelay 100_000
  let initialVty = V.mkVty V.defaultConfig
  buildVty <- initialVty
  void $ customMain buildVty initialVty (Just chan) replayApp (initState seed)

replayApp :: App ReplayState Tick MenuOptions
replayApp =
    App
        { appDraw = drawUI
        , appChooseCursor = neverShowCursor
        , appHandleEvent = replayEventHandler
        , appStartEvent = return ()
        , appAttrMap = const theMap
        }

replayEventHandler :: BrickEvent n Tick -> EventM n (EventList, ReplayState) ()
replayEventHandler (AppEvent Tick) = do
  (evs, rps) <- get
  modify $ \(e, r) -> (e, stepReplayState r)
  when (canExecute evs rps) $ makeMove evs rps
replayEventHandler _ = return ()

canExecute :: [GameEvent] -> ReplayState -> Bool
canExecute [] _ = False
canExecute (GameEvent t0 _: _) (ReplayState _ t1) = t0 == t1

makeMove :: EventList -> ReplayState -> EventM n (EventList, ReplayState) ()
makeMove [] _ = return ()
makeMove (GameEvent _ ev: res) rps = 
  put $ (res, rps {gameState = executeMove ev (gameState rps)})


initState :: StdGen -> ReplayState
initState seed = ReplayState {tickNo=0, gameState=start}
  where start = Playing $ initWorld defaultHeight defaultWidth seed

drawUI :: ReplayState -> [Widget MenuOptions]
drawUI rps = drawGS gs
  where
    gs = gameState rps
