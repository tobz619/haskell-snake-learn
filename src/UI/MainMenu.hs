{-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module UI.MainMenu where

import qualified Brick as B
import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import Brick.Types (Widget)
import qualified Brick.Types as T
import Brick.Util (fg, on)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core
  ( clickable,
    emptyWidget,
    hBox,
    padAll,
    padLeftRight,
    reportExtent,
    txt,
    vBox,
  )
import qualified Brick.Widgets.Dialog as D
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as V
import Lens.Micro.Mtl
import Lens.Micro.TH (makeLenses)

data Choice = Play | HighScores | Options | Quit
  deriving (Eq, Show, Ord)

data DialogState = DialogState
  { _dialog :: D.Dialog Choice Choice,
    _dialogChoice :: Maybe Choice
  }

makeLenses ''DialogState

drawDialogUI :: D.Dialog Choice Choice -> [Widget Choice]
drawDialogUI d = pure $ vBox [ui, controls]
  where
    ui = D.renderDialog d . C.hCenter . padAll 1 $ focus
    controls =
      C.hCenter . B.vLimit 3 . B.hLimit 70 . B.joinBorders . B.freezeBorders . B.borderWithLabel (txt "Controls") . hBox . fmap C.center $
        [ txt $ "\x2190: Left",
          txt $ "\x2192: Right",
          txt $ "\x21b2: Select",
          txt "ESC: Quit"
        ]
    focus =
      maybe
        emptyWidget
        (\n -> (reportExtent *> clickable <*> widgetTitle) $ n)
        $ D.getDialogFocus d

widgetTitle :: Choice -> Widget n
widgetTitle Play = txt "Play the game"
widgetTitle HighScores = txt "View highscores"
widgetTitle Options = txt "Change some options"
widgetTitle Quit = txt "Quit the game"

appEvent :: T.BrickEvent Choice e -> T.EventM Choice DialogState ()
appEvent (T.MouseDown _ V.BLeft _ _) = appEvent (T.VtyEvent (V.EvKey V.KEnter []))
appEvent (T.VtyEvent e) =
  case e of
    V.EvKey V.KEnter [] -> do
      d <- use dialog
      dialogChoice .= (fst <$> D.dialogSelection d)
      M.halt
    V.EvKey V.KEsc [] -> M.halt
    V.EvKey (V.KChar 'c') [V.MCtrl] -> M.halt
    ev -> zoom dialog $ D.handleDialogEvent ev
appEvent _ = return ()

initialState :: DialogState
initialState = DialogState d Nothing
  where
    d = D.dialog (Just $ txt " Main Menu ") (Just (Play, options)) 125
    options =
      [ ("play", Play, Play),
        ("high scores", HighScores, HighScores),
        ("options", Options, Options),
        ("quit game", Quit, Quit)
      ]

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (D.dialogAttr, fg V.white),
      (D.buttonAttr, V.red `on` V.white),
      (D.buttonSelectedAttr, V.white `on` V.red)
    ]

mainMenuApp :: M.App DialogState e Choice
mainMenuApp =
  M.App
    { M.appDraw = drawDialogUI . view dialog,
      M.appChooseCursor = M.neverShowCursor,
      M.appHandleEvent = appEvent,
      M.appStartEvent = return (),
      M.appAttrMap = const theMap
    }

runMainMenu :: V.Vty -> IO (Choice, V.Vty)
runMainMenu vty = do
  (appRes, vty') <- M.customMainWithVty vty (V.mkVty V.defaultConfig) Nothing mainMenuApp initialState
  let res = _dialogChoice appRes
  return (fromMaybe Quit res, vty')
