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
  ( Event (EvKey),
    Key (KDown, KEnter, KEsc, KLeft, KRight, KUp),
  )
import UI.Types


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
    (QuitGame, [K.bind 'q'])
  ]

getKey :: K.KeyConfig KeyEvent -> KeyEvent -> Event
getKey keyConf = maybe (V.EvKey KEsc []) (\(k, mods) -> V.EvKey k (Set.toList mods)) . (K.lookupKeyConfigBindings keyConf >=> handleBindState)
  where
    handleBindState K.Unbound = pure (KEsc, Set.empty)
    handleBindState (K.BindingList xs) = listToMaybe xs >>= \(K.Binding key mods) -> pure (key, mods)

mkHandler :: Text -> (Event -> EventM n s ()) -> K.KeyConfig KeyEvent -> KeyEvent -> K.KeyEventHandler KeyEvent (EventM n s)
mkHandler name handler keyConf keyEvent = K.onEvent keyEvent name (handler (getKey keyConf keyEvent))

mkGameplayHandlers :: [ConfigBinding] -> [(Text, KeyEvent, EventM n s ())] -> [K.KeyEventHandler KeyEvent (EventM n s)]
mkGameplayHandlers override = map (\(t, kev, f) -> gameplayHandler override t (pure f) kev)
  where
    gameplayHandler newConf label f = mkHandler label f (keyConfig newConf)

keyConfig :: [ConfigBinding] -> K.KeyConfig KeyEvent
keyConfig = K.newKeyConfig allKeyEvents keyBindings

gameplayDispatcher :: [ConfigBinding] -> Either [(K.Binding, [K.KeyHandler KeyEvent (EventM n GameState)])] (K.KeyDispatcher KeyEvent (EventM n GameState))
gameplayDispatcher override = K.keyDispatcher (keyConfig override) gameplayHandlers
  where
    gameplayHandlers =
      mkGameplayHandlers
        override
        [ ("up", MoveUp, modify (chDir U)),
          ("down", MoveDown, modify (chDir D)),
          ("left", MoveLeft, modify (chDir L)),
          ("right", MoveRight, modify (chDir R)),
          ("pause", Pause, modify pauseToggle)
        ]
