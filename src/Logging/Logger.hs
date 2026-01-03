{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Logging.Logger where

import Bluefin.Eff (Eff, type (:>))
import Bluefin.Reader (Reader)
import Bluefin.State (State)
import qualified Bluefin.State as BFS
import Brick (BrickEvent (..), get)
import qualified Brick.Keybindings as K
import Brick.Types (EventM)
import Data.Word (Word16)
import GameLogic (Coord)
import Graphics.Vty.CrossPlatform as V ()
import qualified Graphics.Vty.Input as V
import Lens.Micro ((^.))
import Lens.Micro.Mtl ((%=), (.=), use)
import UI.Keybinds
import UI.Types

-- altConfig = []

getKeyEvent :: (t -> Either a (K.KeyDispatcher KeyEvent m)) -> t -> BrickEvent n e -> Maybe KeyEvent
getKeyEvent dispatcher altConfig (VtyEvent (V.EvKey k mods)) = do
  disp <- either (error "Unbound keybind") pure (dispatcher altConfig)
  matchKey k mods disp -- get the event that corresponds to the key on successful execution
  where
    matchKey key modifiers d = do
      let khandler = K.lookupVtyEvent key modifiers d
          event = K.kehEventTrigger . K.khHandler <$> khandler
      case event of
        Just (K.ByKey _) -> Nothing
        Just (K.ByEvent e) -> Just e
        Nothing -> Nothing
getKeyEvent _ _ _ = Nothing

addToLog :: (e :> es) => State EventList e -> TickType -> KeyEvent -> Eff es EventList
addToLog st tick ev = do
  BFS.modify st (GameEvent (TickNumber tick) ev :)
  BFS.get st

addKeyToLog :: (e :> es) => State EventList e -> TickType -> Maybe KeyEvent -> Eff es EventList
addKeyToLog st tick =
  maybe
    (BFS.get st) -- Do nothing if the key is not found
    (addToLog st tick) -- otherwise, write it to the logger

-- Logging Functions
-- | Log a move and add it to the overall EventList as an effect
logMove :: BrickEvent n events -> EventM n GameplayState ()
logMove = handleMovement gameplayDispatcher
  where
    handleMovement :: ([ConfigBinding] -> Either a2 (K.KeyDispatcher KeyEvent m)) -> BrickEvent n e2 -> EventM n GameplayState ()
    handleMovement disp event = do
      let action = getKeyEvent disp [] event
      mapM_ addToLog' action


-- | Log a food getting eaten
logEat :: Coord -> EventM n GameplayState ()
logEat !v2 = addToLog' (FoodEaten v2)


-- | Log the start or end of the game
logGameEnd,logGameStart :: EventM n GameplayState ()
logGameEnd = addToLog' GameEnded
logGameStart = addToLog' GameStarted

addToLog' :: KeyEvent -> EventM n GameplayState ()
addToLog' ev = do
  tick <- use tickNo
  gameLog %= (GameEvent (TickNumber tick) ev :)

-- | Reset the log to an empty list
resetLog :: EventM n GameplayState ()
resetLog = gameLog .= []