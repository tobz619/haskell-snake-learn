{-# LANGUAGE TypeOperators #-}

module Logging.Logger where

import Bluefin
import Bluefin.Compound
import Bluefin.Eff
import Bluefin.IO (IOE)
import Bluefin.Reader
import Bluefin.State
import Bluefin.Stream
import Bluefin.Writer
import qualified Brick.Keybindings as K
import Brick.Types(BrickEvent(VtyEvent), EventM)
import GameLogic (GameState)
import Graphics.Vty.CrossPlatform as V
import qualified Graphics.Vty.Input as V
import UI.Gameplay
import Unsafe.Coerce (unsafeCoerce)

type TickNumber = Int

data LogAction = MovedUp | MovedDown | MovedLeft | MovedRight
  deriving (Show, Eq)

convertToMovement ev@(VtyEvent (V.EvKey k mods)) = do
  handleGameplayEvent' ev
  disp <- case gameplayDispatcher [] of
    Right disp -> return disp
    Left _ -> undefined
  _ <- K.handleKey disp k mods
  return ()

convertToMovement _ = return ()

-- | Pairing of tick events to significant moves
data GameEvent = GameEvent TickNumber LogAction

type EventHistory e = Writer EventList e

type EventList = [GameEvent]

data Logger n a es = Logger
  { log :: Writer EventList es,
    gameState :: Reader GameplayState es
  }

appendGameEvent :: EventList -> (TickNumber, LogAction) -> EventList
appendGameEvent gs = (: gs) . uncurry GameEvent

incTicks :: (e :> es) => State TickNumber e -> Eff es ()
incTicks st = modify st (+ 1)

addToLog :: (e :> es) => EventHistory e -> GameEvent -> Eff es ()
addToLog hist ev = tell hist (pure ev)