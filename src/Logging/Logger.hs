{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Logging.Logger where

import Bluefin.Compound (Handle (..), useImplIn)
import Bluefin.Eff (Eff, (:&), type (:>))
import Bluefin.Reader (Reader, runReader)
import Bluefin.Writer (Writer, execWriter, tell)
import Brick (BrickEvent (..))
import qualified Brick.Keybindings as K
import Graphics.Vty.CrossPlatform as V ()
import qualified Graphics.Vty.Input as V
import UI.Keybinds

type TickNumber = Int

-- | Pairing of tick events to significant moves
data GameEvent = GameEvent TickNumber KeyEvent
  deriving (Show)

type EventList = [GameEvent]

data Logger g e = Logger
  { writeLog :: Writer EventList e,
    onGameplayState :: Reader g e
  }

getKeyEvent :: (t -> Either a (K.KeyDispatcher KeyEvent m)) -> t -> BrickEvent n e -> Maybe KeyEvent
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

addToLog :: (e :> es) => Writer EventList e -> TickNumber -> Maybe KeyEvent -> Eff es ()
addToLog strm tick =
  maybe
    (pure ()) -- Do nothing if the key is not found
    (tell strm . pure . GameEvent tick) -- otherwise, write it to the logger

runLogger :: (forall e1. Logger g e1 -> Eff (e1 :& es) r) -> g -> Eff es [GameEvent]
runLogger f gps = runReader gps $ \rea -> do
  execWriter $ \y -> do
    useImplIn f (Logger (mapHandle y) (mapHandle rea))
