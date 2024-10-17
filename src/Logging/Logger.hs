module Logging.Logger where

import Brick.Types(BrickEvent)
import Control.Monad.State
import UI.Gameplay

type TickNumber = Int

data GameEvent n e = GameEvent TickNumber (BrickEvent n e)

type EventList n e = [GameEvent n e]

type GameCounter n e = State (EventList n e)
