{-# LANGUAGE TypeOperators #-}

module Logging.Logger where

import Bluefin
import Bluefin.Compound
import Bluefin.Eff
import Bluefin.State
import Bluefin.Stream
import Bluefin.Writer
import Brick.Types (EventM)
import GameLogic (GameState)
import UI.Gameplay

type TickNumber = Int

-- | Pairing of tick events to significant moves
data GameEvent n a = GameEvent TickNumber (EventM n GameState a)

newtype EventHistory n a e = EventHistory (Writer (EventList n a) e)

type EventList n a = [GameEvent n a]

newtype GameCounter e = GameCounter (State TickNumber e)

-- newtype Event n s a = Event { runEvent :: ReaderT }

data Logger n a es = Logger
  { log :: Writer (EventList n a) es,
    events :: Stream (GameEvent n a) es,
    ticks :: State TickNumber es
  }

appendGameEvent :: [GameEvent n a] -> (TickNumber, EventM n GameState a) -> [GameEvent n a]
appendGameEvent gs = (: gs) . uncurry GameEvent

incTicks :: (e :> es) => GameCounter e -> Eff es ()
incTicks (GameCounter st) = modify st (+ 1)

addToLog :: (e :> es) => EventHistory n a e -> GameEvent n a -> Eff es ()
addToLog (EventHistory hist) ev = tell hist (pure ev)

