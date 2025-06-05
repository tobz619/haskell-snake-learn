{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module UI.ReplayPlayer where

import Brick
    ( App(..),
      BrickEvent(AppEvent, VtyEvent),
      EventM,
      customMain,
      neverShowCursor,
      modify )
import Brick.BChan (newBChan, writeBChan)
import Brick.Main (halt)
import Brick.Types (Widget)
import Brick.Widgets.Center (center)
import Brick.Widgets.Core
import Control.Concurrent (MVar, forkIO, modifyMVar_, newMVar, readMVar, threadDelay)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as Text
import qualified Data.Vector.Strict as Vec
import GameLogic (GameState (GameOver, NewHighScore), pauseToggle)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as V
import Linear.V2 (V2 (..))
import Logging.Replay (ReplayState (..), handleSpeed, initState)
import System.Random (StdGen, mkStdGen)
import UI.Gameplay (drawGS, theMap)
import UI.Types
import Data.List (uncons)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)


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
    { appDraw = flip drawUI evs,
      appChooseCursor = neverShowCursor,
      appHandleEvent = \brickEv -> replayEventHandler brickEv evs speedMod,
      appStartEvent = replayEventHandler (AppEvent Tick) evs speedMod,
      appAttrMap = const theMap
    }

replayEventHandler :: BrickEvent n Tick -> InputList -> MVar Float -> EventM n ReplayState ()
replayEventHandler (VtyEvent (V.EvKey (V.KChar 'p') [])) _ _ = do
  modify (\r -> r {rGameState = pauseToggle (rGameState r)} )
  -- gs <- gets rGameState
  -- case gs of
  --   Playing w -> stepGameState gs

replayEventHandler (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) _ _ = halt
replayEventHandler (VtyEvent (V.EvKey k [V.MCtrl])) _ _ = do
  case k of
    V.KLeft -> modify prevCheckPoint
    V.KRight -> modify nextCheckPoint
    _ -> pure ()
replayEventHandler (VtyEvent (V.EvKey k _)) _ mSpeedMod = liftIO $ do
  case k of
    V.KChar 'n' -> modifyMVar_ mSpeedMod (pure . normalSpeed)
    V.KChar 'r' -> modifyMVar_ mSpeedMod (pure . negate)
    V.KLeft -> modifyMVar_ mSpeedMod (pure . speedDown)
    V.KRight -> modifyMVar_ mSpeedMod (pure . speedUp)
    _ -> pure ()
replayEventHandler (AppEvent Tick) evList mSpeedMod = do
  speed <- liftIO $ readMVar mSpeedMod
  handleSpeed speed evList
replayEventHandler _ _ _ = pure ()



drawUI :: ReplayState -> InputList -> [Widget MenuOptions]
drawUI rps evList = [drawDebug rps evList] <> (center <$> drawGS gs)
  where
    gs = case rGameState rps of
      NewHighScore w -> GameOver w -- Short circuit the GameState to GameOver so it renders properly
      g -> g

drawDebug :: ReplayState -> InputList -> Widget n
drawDebug gps evList = currentTick <=> nextEvTick <=> evIndex <=> rIndex <=> arrLen <=> evLen
  where
    eIndex = (\(EvNumber x) -> x) (rEvIndex gps)
    evNumber (GameEvent t _) = t
    currentTick = hLimit 15 $ hBox [txt "Tick: ", txt $ Text.pack $ show $ rTickNo gps]
    evIndex = hLimit 15 $ hBox [txt "Ev Index: ", txt $ Text.pack $ show $ rEvIndex gps]
    rIndex = hLimit 15 $ hBox [txt "Rew Index: ", txt $ Text.pack $ show $ rRewindIndex gps]
    nextEvTick = hLimit 15 $ vBox [txt "Nx Ev Tick: ", txt $ Text.pack $ show $ evNumber <$> evList Vec.!? eIndex]
    arrLen = hLimit 15 $ hBox [txt "Arr Len: ", txt $ Text.pack $ show $ either Vec.length length (rGameStateVec gps)]
    evLen = hLimit 15 $ hBox [txt "Ev Len: ", txt $ Text.pack $ show $ Vec.length evList]

-- currentLog = vLimit 20 $ hLimit 45 $ vBox $ txt . Text.pack . show <$> take 20 (rEventList gps)

nextCheckPoint :: ReplayState -> ReplayState
nextCheckPoint rps = let tNow = rTickNo rps
                         cpVec = rCheckPoint rps
                         finder _ (Just x) = Just x
                         finder r Nothing
                          | rTickNo r > tNow  = Just (r {rCheckPoint = cpVec, rGameStateVec = Right []})
                          | otherwise = Nothing
                      in fromMaybe rps $
                          Vec.foldr finder Nothing cpVec


prevCheckPoint :: ReplayState -> ReplayState
prevCheckPoint rps = let tNow = rTickNo rps
                         cpVec = rCheckPoint rps
                         finder _ (Just x) = Just x
                         finder r Nothing
                          | tNow - rTickNo r < 80 = Just (r {rCheckPoint = cpVec, rGameStateVec = Right []})
                          | otherwise = Nothing
                      in fromMaybe rps $
                          Vec.foldr finder Nothing cpVec


replayExample :: IO ()
replayExample = do
  let evs = mkInputList evs3
  speed <- newMVar 1
  runReplayApp (mkStdGen seed3) evs speed

-- where
--   moves = [1, 3, 8, 10, 1, 11, 14, 1, 1]
--   events =
--     zipWith
--       (GameEvent . TickNumber)
--       (scanl' (+) 1 moves)
--       [MoveRight, MoveDown, MoveLeft, MoveUp, MoveRight, MoveDown, MoveRight, MoveDown, MoveLeft]

readReplayFromFile :: IO ()
readReplayFromFile = do
  contents <- lines <$> readFile "Seed-Events"
  let seed = fromMaybe (error "Failed to read seed") (readMaybe @SeedType . fst =<< uncons contents)
      evs = mkInputList . mkEvs $ read @[GameEvent] $ contents !! 1
  speed <- newMVar 1
  runReplayApp (mkStdGen seed) evs speed


speedUp, speedDown, normalSpeed :: Float -> Float
speedUp x
  | x >= 16 = x
  | otherwise = x * 2
speedDown x
  | abs x <= (1 / 8) = x
  | otherwise = x / 2
normalSpeed = const 1

mkEvs = reverse  . filter (\(GameEvent _ x) -> isMovement x)
  where
    isMovement x = x `elem` [MoveUp, MoveDown, MoveLeft, MoveRight]

evs2 :: [GameEvent]
evs2 =
  mkEvs
    [GameEvent 534 GameEnded, GameEvent 533 (FoodEaten (V2 10 19)), GameEvent 525 MoveUp, GameEvent 519 MoveLeft, GameEvent 513 MoveDown, GameEvent 511 (FoodEaten (V2 14 17)), GameEvent 500 MoveRight, GameEvent 499 MoveUp, GameEvent 484 (FoodEaten (V2 18 16)), GameEvent 483 MoveLeft, GameEvent 476 MoveUp, GameEvent 470 MoveRight, GameEvent 462 MoveUp, GameEvent 456 (FoodEaten (V2 19 1)), GameEvent 456 MoveLeft, GameEvent 450 MoveDown, GameEvent 435 MoveRight, GameEvent 432 MoveUp, GameEvent 424 MoveLeft, GameEvent 423 (FoodEaten (V2 12 5)), GameEvent 415 MoveDown, GameEvent 410 MoveRight, GameEvent 409 (FoodEaten (V2 7 12)), GameEvent 400 MoveUp, GameEvent 392 MoveLeft, GameEvent 384 MoveDown, GameEvent 382 (FoodEaten (V2 13 11)), GameEvent 381 MoveRight, GameEvent 374 MoveUp, GameEvent 364 MoveRight, GameEvent 360 (FoodEaten (V2 2 8)), GameEvent 360 MoveDown, GameEvent 356 MoveLeft, GameEvent 349 MoveDown, GameEvent 346 MoveRight, GameEvent 338 MoveUp, GameEvent 328 MoveLeft, GameEvent 321 MoveDown, GameEvent 315 MoveRight, GameEvent 313 (FoodEaten (V2 7 12)), GameEvent 312 MoveUp, GameEvent 306 MoveLeft, GameEvent 297 MoveUp, GameEvent 291 MoveRight, GameEvent 290 (FoodEaten (V2 7 3)), GameEvent 287 MoveDown, GameEvent 278 MoveLeft, GameEvent 273 MoveDown, GameEvent 271 (FoodEaten (V2 14 11)), GameEvent 267 MoveRight, GameEvent 262 MoveUp, GameEvent 257 MoveLeft, GameEvent 256 (FoodEaten (V2 15 7)), GameEvent 251 MoveDown, GameEvent 248 MoveRight, GameEvent 241 (FoodEaten (V2 12 5)), GameEvent 241 MoveUp, GameEvent 235 MoveRight, GameEvent 231 MoveDown, GameEvent 229 (FoodEaten (V2 8 9)), GameEvent 226 MoveLeft, GameEvent 222 MoveDown, GameEvent 218 MoveRight, GameEvent 212 MoveUp, GameEvent 208 MoveLeft, GameEvent 207 (FoodEaten (V2 11 8)), GameEvent 201 MoveDown, GameEvent 197 MoveRight, GameEvent 192 MoveDown, GameEvent 190 (FoodEaten (V2 9 19)), GameEvent 184 MoveLeft, GameEvent 169 MoveUp, GameEvent 164 (FoodEaten (V2 10 4)), GameEvent 163 MoveRight, GameEvent 156 MoveDown, GameEvent 149 (FoodEaten (V2 16 11)), GameEvent 148 MoveLeft, GameEvent 142 MoveDown, GameEvent 140 MoveRight, GameEvent 132 MoveUp, GameEvent 122 MoveRight, GameEvent 118 MoveDown, GameEvent 116 (FoodEaten (V2 7 13)), GameEvent 115 MoveLeft, GameEvent 105 MoveUp, GameEvent 94 MoveLeft, GameEvent 93 (FoodEaten (V2 19 4)), GameEvent 85 MoveDown, GameEvent 81 MoveRight, GameEvent 76 MoveDown, GameEvent 73 (FoodEaten (V2 12 17)), GameEvent 72 MoveRight, GameEvent 66 MoveUp, GameEvent 59 (FoodEaten (V2 18 11)), GameEvent 58 MoveLeft, GameEvent 48 MoveUp, GameEvent 31 (FoodEaten (V2 2 1)), GameEvent 30 MoveRight, GameEvent 26 MoveDown, GameEvent 12 (FoodEaten (V2 15 5)), GameEvent 11 MoveLeft, GameEvent 6 MoveDown, GameEvent 0 MoveRight]

seed2 :: SeedType
seed2 = 9157570410566699885

seed3 = 8708303021790635255

evs3 :: [GameEvent]
evs3 =
  mkEvs
    [GameEvent 574 GameEnded, GameEvent 568 MoveRight, GameEvent 563 MoveUp, GameEvent 559 (FoodEaten (V2 10 2)), GameEvent 557 MoveLeft, GameEvent 540 MoveDown, GameEvent 531 MoveRight, GameEvent 528 MoveUp, GameEvent 527 (FoodEaten (V2 4 16)), GameEvent 522 MoveLeft, GameEvent 517 MoveUp, GameEvent 510 MoveRight, GameEvent 508 (FoodEaten (V2 1 8)), GameEvent 504 MoveUp, GameEvent 490 MoveLeft, GameEvent 486 MoveUp, GameEvent 475 MoveRight, GameEvent 475 (FoodEaten (V2 4 2)), GameEvent 473 MoveDown, GameEvent 467 MoveLeft, GameEvent 459 MoveDown, GameEvent 452 MoveRight, GameEvent 448 (FoodEaten (V2 3 6)), GameEvent 444 MoveUp, GameEvent 436 MoveLeft, GameEvent 434 MoveDown, GameEvent 427 MoveRight, GameEvent 426 (FoodEaten (V2 4 7)), GameEvent 423 MoveDown, GameEvent 408 MoveLeft, GameEvent 404 (FoodEaten (V2 19 14)), GameEvent 400 MoveDown, GameEvent 396 MoveRight, GameEvent 382 MoveUp, GameEvent 380 (FoodEaten (V2 12 3)), GameEvent 374 MoveRight, GameEvent 370 MoveDown, GameEvent 366 MoveLeft, GameEvent 364 (FoodEaten (V2 11 10)), GameEvent 359 MoveDown, GameEvent 349 MoveRight, GameEvent 345 (FoodEaten (V2 1 9)), GameEvent 339 MoveUp, GameEvent 329 MoveLeft, GameEvent 326 MoveUp, GameEvent 320 (FoodEaten (V2 4 1)), GameEvent 319 MoveRight, GameEvent 315 MoveDown, GameEvent 302 MoveLeft, GameEvent 301 MoveUp, GameEvent 291 (FoodEaten (V2 6 4)), GameEvent 289 MoveRight, GameEvent 286 MoveUp, GameEvent 278 (FoodEaten (V2 14 1)), GameEvent 277 MoveLeft, GameEvent 267 MoveDown, GameEvent 258 MoveRight, GameEvent 254 (FoodEaten (V2 5 6)), GameEvent 249 MoveUp, GameEvent 237 MoveLeft, GameEvent 237 (FoodEaten (V2 17 3)), GameEvent 229 MoveDown, GameEvent 215 MoveRight, GameEvent 211 MoveDown, GameEvent 208 (FoodEaten (V2 7 14)), GameEvent 206 MoveLeft, GameEvent 195 MoveUp, GameEvent 186 (FoodEaten (V2 18 3)), GameEvent 184 MoveLeft, GameEvent 183 MoveUp, GameEvent 169 MoveRight, GameEvent 169 (FoodEaten (V2 5 3)), GameEvent 165 MoveDown, GameEvent 163 MoveLeft, GameEvent 158 MoveDown, GameEvent 155 MoveRight, GameEvent 151 MoveUp, GameEvent 149 (FoodEaten (V2 7 7)), GameEvent 146 MoveLeft, GameEvent 137 MoveDown, GameEvent 131 (FoodEaten (V2 2 16)), GameEvent 129 MoveRight, GameEvent 124 MoveUp, GameEvent 113 (FoodEaten (V2 13 11)), GameEvent 108 MoveLeft, GameEvent 102 MoveDown, GameEvent 98 (FoodEaten (V2 12 17)), GameEvent 89 MoveRight, GameEvent 75 MoveUp, GameEvent 74 (FoodEaten (V2 6 3)), GameEvent 72 MoveLeft, GameEvent 59 MoveDown, GameEvent 48 MoveLeft, GameEvent 45 MoveDown, GameEvent 45 (FoodEaten (V2 17 19)), GameEvent 38 MoveRight, GameEvent 35 MoveUp, GameEvent 31 MoveRight, GameEvent 29 MoveDown, GameEvent 17 MoveLeft, GameEvent 15 (FoodEaten (V2 19 15)), GameEvent 9 MoveUp, GameEvent 0 MoveRight, GameEvent 0 GameStarted]



  -- [xs Vec.!! y | y <- [0, n .. length xs - 1]]

