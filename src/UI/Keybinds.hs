{-# LANGUAGE OverloadedStrings #-}

module UI.Keybinds where

import qualified Brick.Keybindings as K
import Brick.Types (EventM, modify)
import Control.Monad ((>=>))
import Data.Maybe (listToMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import GameLogic (Direction (..), GameState, chDir, pauseToggle)
import Graphics.Vty as V
    ( Event(EvKey), Key(KEsc, KUp, KDown, KLeft, KRight, KEnter) )

data KeyEvent = MoveUp | MoveDown | MoveLeft | MoveRight | Back | Select | Pause | Stop | Halt | Quit
  deriving (Show, Eq, Ord)

allKeyEvents :: K.KeyEvents KeyEvent
allKeyEvents =
  K.keyEvents
    [ ("up", MoveUp),
      ("down", MoveDown),
      ("left", MoveLeft),
      ("right", MoveRight),
      ("back", Back),
      ("select", Select),
      ("pause", Pause),
      ("halt", Halt)
    ]

keyBindings :: [(KeyEvent, [K.Binding])]
keyBindings =
  [ (MoveUp, [K.bind V.KUp]),
    (MoveDown, [K.bind V.KDown]),
    (MoveLeft, [K.bind V.KLeft]),
    (MoveRight, [K.bind V.KRight]),
    (Select, [K.bind V.KEnter]),
    (Back, [K.bind V.KEsc]),
    (Pause, [K.bind 'p']),
    (Halt, [K.ctrl 'c']),
    (Quit, [K.bind 'q'])
  ]

getKey :: K.KeyConfig KeyEvent -> KeyEvent -> Event
getKey keyConf = maybe (V.EvKey KEsc []) (\(k, mods) -> V.EvKey k (Set.toList mods)) . (K.lookupKeyConfigBindings keyConf >=> handleBindState)
  where
    handleBindState K.Unbound = pure (KEsc, Set.empty)
    handleBindState (K.BindingList xs) = listToMaybe xs >>= \(K.Binding key mods) -> pure (key, mods)

mkHandler :: Text -> (Event -> EventM n s ()) -> K.KeyConfig KeyEvent -> KeyEvent -> K.KeyEventHandler KeyEvent (EventM n s)
mkHandler name handler keyConf keyEvent = K.onEvent keyEvent name (handler (getKey keyConf keyEvent))

mkGameplayHandlers :: [(KeyEvent, K.BindingState)] -> [(Text, KeyEvent, EventM n s ())] -> [K.KeyEventHandler KeyEvent (EventM n s)]
mkGameplayHandlers new = map (\(t, kev, f) -> gameplayHandler new t (pure f) kev)
  where
    gameplayHandler newConf label f = mkHandler label f (keyConfig newConf)

keyConfig :: [(KeyEvent, K.BindingState)] -> K.KeyConfig KeyEvent
keyConfig = K.newKeyConfig allKeyEvents keyBindings

gameplayDispatcher :: [(KeyEvent, K.BindingState)] -> Either [(K.Binding, [K.KeyHandler KeyEvent (EventM n GameState)])] (K.KeyDispatcher KeyEvent (EventM n GameState))
gameplayDispatcher new = K.keyDispatcher (keyConfig new) gameplayHandlers
  where
    gameplayHandlers =
      mkGameplayHandlers
        new
        [ ("up", MoveUp, modify (chDir U)),
          ("down", MoveDown, modify (chDir D)),
          ("left", MoveLeft, modify (chDir L)),
          ("right", MoveRight, modify (chDir R)),
          ("pause", Pause, modify pauseToggle)
        ]