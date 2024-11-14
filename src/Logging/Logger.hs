{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Logging.Logger where

import Bluefin
import Bluefin.Compound (Handle (..), useImplIn, useImpl)
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
import UI.Keybinds

type TickNumber = Int

-- | Pairing of tick events to significant moves
data GameEvent = GameEvent TickNumber KeyEvent
  deriving Show

type EventList = [GameEvent]

data Logger g e = Logger
  { writeLog :: Stream GameEvent e,
    onGameplayState :: Reader g e
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

addToLog :: (e :> es) => Stream GameEvent e -> TickNumber -> Maybe KeyEvent -> Eff es ()
addToLog strm tick = maybe
                      (pure ()) -- Do nothing if the key is not found
                      (yield strm . GameEvent tick) -- otherwise, write it to the logger

runLogger :: (e1 :> es) => Stream GameEvent e1 -> (forall e. Logger g e -> Eff (e :& es) r) -> g -> Eff es r
runLogger y f gps  = do
    runReader gps $ \rea -> do
      useImplIn f $ Logger (mapHandle y) (mapHandle rea)


