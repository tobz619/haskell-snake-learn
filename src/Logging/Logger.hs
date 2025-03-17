{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Logging.Logger where

import Bluefin.Eff (Eff, type (:>))
import Bluefin.Reader (Reader)
import Brick (BrickEvent (..))
import qualified Brick.Keybindings as K
import Graphics.Vty.CrossPlatform as V ()
import qualified Graphics.Vty.Input as V
import UI.Keybinds
import Data.Word (Word16)
import Bluefin.State ( State, modify, get )

newtype TickNumber = TickNumber TickType deriving newtype (Eq, Show)
type TickType = Word16

-- | Pairing of tick events to significant moves
data GameEvent = GameEvent TickNumber KeyEvent
  deriving (Show)

type EventList = [GameEvent]

data Logger g e = Logger (State EventList e) (Reader g e)

getKeyEvent :: (t -> Either a (K.KeyDispatcher KeyEvent m)) -> t -> BrickEvent n e -> Maybe KeyEvent
getKeyEvent dispatcher altConfig (VtyEvent (V.EvKey k mods)) = do
  disp <- case dispatcher altConfig of
    Right disp -> return disp
    Left _ -> error "Unbound keybind"
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
  modify st (GameEvent (TickNumber tick) ev :)
  get st

addKeyToLog :: (e :> es) => State EventList e -> TickType -> Maybe KeyEvent -> Eff es EventList
addKeyToLog st tick =
  maybe
    (get st) -- Do nothing if the key is not found
    (addToLog st tick) -- otherwise, write it to the logger


