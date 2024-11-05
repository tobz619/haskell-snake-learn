{-# LANGUAGE TypeOperators #-}

module Logging.Logger where

import Bluefin
import Bluefin.Eff (Eff, (:&), type (:>))
import Bluefin.IO (IOE)
import Bluefin.Reader (Reader, ask)
import Bluefin.State
import Bluefin.Stream
import Bluefin.Writer (Writer, tell)
-------------------
import Brick (BrickEvent (..))
import qualified Brick.Keybindings as K
-------------------
import Graphics.Vty.CrossPlatform as V ()
import qualified Graphics.Vty.Input as V
import Lens.Micro ((^.))
-------------------
import UI.Gameplay (GameplayState, Tick (..), tickNo)
import UI.Keybinds

type TickNumber = Int

-- | Pairing of tick events to significant moves
data GameEvent = GameEvent TickNumber KeyEvent

type EventHistory e = Writer EventList e

type EventList = [GameEvent]

data Logger n a es = Logger
  { log :: Writer EventList es,
    gState :: Reader GameplayState es
  }

addToLog :: (e :> es) => EventHistory e -> TickNumber -> Maybe KeyEvent -> Eff es ()
addToLog hist tn (Just ev) = tell hist (pure (GameEvent tn ev))
addToLog _ _ Nothing = pure ()

getKeyEvent :: BrickEvent n e -> Maybe KeyEvent
getKeyEvent (VtyEvent (V.EvKey k mods)) = do
  disp <- case gameplayDispatcher [] of
    Right disp -> return disp
    Left _ -> undefined
  matchKey k mods disp -- get the event that corresponds to the key on successful execution
  where
    matchKey key modifiers d = do
      let khandler = K.lookupVtyEvent key modifiers d
          event = K.kehEventTrigger . K.khHandler <$> khandler
      case event of
        Just (K.ByKey _) -> Nothing
        Just (K.ByEvent e) -> Just e
        Nothing -> Nothing
getKeyEvent _ = Nothing

handleMovement :: (e :> es) => BrickEvent n Tick -> Logger n a e -> Eff es ()
handleMovement ev (Logger writ readstate) = do
  gameplaystate <- ask readstate
  let logaction = getKeyEvent ev
      tick = gameplaystate ^. tickNo
  addToLog writ tick logaction
