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
import Brick.Types (BrickEvent (VtyEvent), EventM)
import Control.Applicative ((<|>))
import GameLogic (GameState)
import Graphics.Vty.CrossPlatform as V
import qualified Graphics.Vty.Input as V
import Lens.Micro
import UI.Gameplay
import UI.Keybinds

type TickNumber = Int

-- | Pairing of tick events to significant moves
data GameEvent = GameEvent TickNumber KeyEvent

type EventHistory e = Writer EventList e

type EventList = [GameEvent]

data Logger n a es = Logger
  { log :: Writer EventList es,
    gameState :: Reader GameplayState es
  }

appendGameEvent :: EventList -> (TickNumber, KeyEvent) -> EventList
appendGameEvent gs = (: gs) . uncurry GameEvent

addToLog :: (e :> es) => EventHistory e -> GameEvent -> Eff es ()
addToLog hist ev = tell hist (pure ev)

logMovement :: BrickEvent n1 e -> EventM n1 GameState (Maybe KeyEvent)
logMovement ev@(VtyEvent (V.EvKey k mods)) = do
  handleGameplayEvent' ev
  disp <- case gameplayDispatcher [] of
    Right disp -> return disp
    Left _ -> undefined
  b <- K.handleKey disp k mods -- execute the key and see if it's handled
  if b
    then matchKey k mods disp -- get the event that corresponds to the key on successful execution
    else return Nothing
  where
    matchKey key modifiers d = do
      let khandler = K.lookupVtyEvent key modifiers d
          event = K.kehEventTrigger . K.khHandler <$> khandler
      case event of
        Just (K.ByKey _) -> return Nothing
        Just (K.ByEvent e) -> return $ Just e
        Nothing -> return Nothing
logMovement _ = return Nothing

logg ev (Logger writ readstate) = handleMovement ev <|> handleTick ev
  where
    handleMovement e = do
      gameplaystate <- ask readstate
      logaction <- logMovement e
      let tick = gameplaystate ^. tickNo
      addToLog writ (GameEvent tick logaction)
