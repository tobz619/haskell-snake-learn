{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Logging.Logger where

import Bluefin
import Bluefin.Compound (Handle (..), useImplIn)
import Bluefin.Eff (Eff, runPureEff, (:&), type (:>))
import Bluefin.IO (IOE)
import Bluefin.Reader (Reader, ask, runReader)
import Bluefin.State
import Bluefin.Stream
import Bluefin.Writer (Writer, execWriter, runWriter, tell)
import Brick (BrickEvent (..))
import qualified Brick.Keybindings as K
import Brick.Types (EventM)
import GameLogic (GameState)
import Graphics.Vty.CrossPlatform as V ()
import qualified Graphics.Vty.Input as V
import Lens.Micro ((^.))
import UI.Gameplay (GameplayState, Tick, tickNo)
import UI.Keybinds

type TickNumber = Int

-- | Pairing of tick events to significant moves
data GameEvent = GameEvent TickNumber KeyEvent

type EventList = [GameEvent]

data Logger e = Logger
  { writeLog :: Writer EventList e,
    onGameplayState :: Reader GameplayState e
  }

getKeyEvent dispatcher altConfig (VtyEvent (V.EvKey k mods)) = do
  disp <- case dispatcher altConfig of
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
getKeyEvent _ _ _ = Nothing

handleMovement disp altConfig ev (Logger writ readstate) = do
  gameplaystate <- ask readstate
  let logaction = getKeyEvent disp altConfig ev
      tick = gameplaystate ^. tickNo
  addToLog writ tick logaction

addToLog :: (e :> es) => Writer EventList e -> TickNumber -> Maybe KeyEvent -> Eff es ()
addToLog writ tick = maybe (pure ()) (tell writ . pure . GameEvent tick)

runLogger :: (forall e. Logger e -> Eff (e :& es) r) -> GameplayState -> Eff es EventList
runLogger f gps =
  execWriter $ \writ -> do
    runReader gps $ \rea -> do
      useImplIn f $ Logger (mapHandle writ) (mapHandle rea)


-- execLogger (Logger l g) = runPureEff $ execWriter l $ ()

-- example :: GameplayState -> BrickEvent n e -> ()
-- example gps ev = runPureEff $ flip runLogger gps $ \l -> do
--   handleMovement gameplayDispatcher ev l