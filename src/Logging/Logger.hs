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
import Bluefin.IO (IOE)
import Unsafe.Coerce (unsafeCoerce)

type TickNumber = Int

data LogAction = MovedUp | MovedDown | MovedLeft | MovedRight 
  deriving (Show, Eq)

convertToMovement :: EventM n GameState a -> LogAction
convertToMovement _ = undefined

-- | Pairing of tick events to significant moves
data GameEvent = GameEvent TickNumber LogAction

type EventHistory e = Writer EventList e

type EventList = [GameEvent]

data Event n s a es = Event (EventM n s a) es

data Logger n a es = Logger
  { log :: Writer EventList es,
    events :: Stream GameEvent es,
    ticks :: State TickNumber es
  }

appendGameEvent :: EventList -> (TickNumber, LogAction) -> EventList
appendGameEvent gs = (: gs) . uncurry GameEvent

incTicks :: (e :> es) => State TickNumber e -> Eff es ()
incTicks st = modify st (+ 1)

addToLog :: (e :> es) => EventHistory e -> GameEvent -> Eff es ()
addToLog hist ev = tell hist (pure ev)

getEvents = runEff $ \ioe -> yieldToList $ \s -> do
  (tick , action) <- undefined
  let conv = convertToMovement action
  yield s (GameEvent tick conv)

eventMtoEvent :: EventM n s a -> Event n s a e
eventMtoEvent = unsafeCoerce

convertEvent :: Event n s a e -> LogAction
convertEvent ev = case runEvent ev of
  undefined -> undefined