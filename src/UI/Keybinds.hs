{-# LANGUAGE OverloadedStrings #-}

module UI.Keybinds where

import qualified Brick.Keybindings as K
import Brick.Types(EventM, modify, BrickEvent (..))
import GameLogic(GameState, chDir, Direction (..), pauseToggle)
import Brick.Widgets.Dialog(Dialog(..))
import Brick.Widgets.Dialog as D
import Graphics.Vty as V

data KeyEvent = MoveUp | MoveDown | MoveLeft | MoveRight | Back | Select | Pause | Stop | Halt
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
    (Halt, [K.ctrl 'c'])
  ]

keyConfig :: [(KeyEvent, K.BindingState)] -> K.KeyConfig KeyEvent
keyConfig = K.newKeyConfig allKeyEvents keyBindings

dialogDispatcher :: [(KeyEvent, K.BindingState)] -> Either [(K.Binding, [K.KeyHandler KeyEvent (EventM n (Dialog a n))])] (K.KeyDispatcher KeyEvent (EventM n (Dialog a n)))
dialogDispatcher new = K.keyDispatcher (keyConfig new) [upHandler, downHandler]
  where
    upHandler = K.onEvent MoveUp "up" (D.handleDialogEvent (V.EvKey V.KUp []))
    downHandler = K.onEvent MoveDown "down" (D.handleDialogEvent (V.EvKey V.KDown []))

gameplayDispatcher :: [(KeyEvent, K.BindingState)] -> Either [(K.Binding, [K.KeyHandler KeyEvent (EventM n GameState)])] (K.KeyDispatcher KeyEvent (EventM n GameState))
gameplayDispatcher new = K.keyDispatcher (keyConfig new) [upHandler, downHandler, leftHandler, rightHandler, pauseHandler]
  where
    upHandler = K.onEvent MoveUp "up" (modify (chDir U))
    downHandler = K.onEvent MoveDown "down" (modify (chDir D))
    leftHandler = K.onEvent MoveLeft "left" (modify (chDir L))
    rightHandler = K.onEvent MoveRight "right" (modify (chDir R))
    pauseHandler = K.onEvent Pause "pause" (modify pauseToggle)

handleGameplayEvent' :: BrickEvent n1 e -> EventM n2 GameState ()
handleGameplayEvent' (VtyEvent (V.EvKey k mods)) = do
  disp <- case gameplayDispatcher [] of
    Right disp -> return disp
    Left _ -> undefined

  _ <- K.handleKey disp k mods
  return ()
handleGameplayEvent' _ = return ()