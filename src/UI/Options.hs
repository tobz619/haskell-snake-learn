{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module UI.Options where

import Brick (BrickEvent (..), EventM, Widget, fill, gets, hLimit, on, txt, vLimit, zoom, (<+>), (<=>))
import qualified Brick as B
import Brick.AttrMap (attrMap)
import Brick.Focus (focusRingCursor)
import Brick.Forms (Form, (@@=))
import qualified Brick.Forms as F
import Brick.Main (App (..), neverShowCursor)
import qualified Brick.Main as M
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core (Padding (..), padBottom)
import qualified Brick.Widgets.Edit as E
import Control.Monad.IO.Class (liftIO)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as V
import Lens.Micro.TH (makeLenses)
import Options.Options

data FormParams = OnlineField | Save | Cancel
  deriving (Eq, Ord, Show)

options :: V.Vty -> IO V.Vty
options vty = do
  opts <- either (const defaultOptions) id <$> openOpts
  snd <$> M.customMainWithVty vty (V.mkVty V.defaultConfig) Nothing optionsApp (mkForm opts)
  where
    defaultOptions = Options False

optionsApp :: App (Form Options e FormParams) e FormParams
optionsApp =
  App
    { appDraw = drawUI,
      appChooseCursor = focusRingCursor F.formFocus,
      appHandleEvent = eventHandler,
      appStartEvent = pure (),
      appAttrMap = const theMap
    }

mkForm :: Options -> Form Options e FormParams
mkForm =
  let label s w = padBottom (Pad 1) $ (vLimit 1 . hLimit 15 $ txt s <+> fill ' ') <+> w
   in F.newForm
        [ label "Online" @@= F.checkboxField online OnlineField "Play online?"
        ]

drawUI :: Form Options e FormParams -> [Widget FormParams]
drawUI f =
  [ (C.center . hLimit 75 . vLimit 100 . B.joinBorders) ((B.border (txt "Options" <=> B.hBorder <=> F.renderForm f) <+> controls))
  ]
  where
    controls =
      B.borderWithLabel (txt "Controls") . B.vBox $
        [ txt "Shift + TAB: Previous option",
          txt "TAB: Next option",
          txt "SPACE: Toggle option",
          txt "\x21b2: Exit and save settings",
          txt "ESC: Exit without saving"
        ]

eventHandler :: BrickEvent FormParams e -> EventM FormParams (Form Options e FormParams) ()
eventHandler (VtyEvent (V.EvKey V.KEsc [])) = M.halt
eventHandler (VtyEvent (V.EvKey (V.KChar 'q') [])) = M.halt
eventHandler (VtyEvent (V.EvKey V.KEnter [])) = do
  opts <- gets F.formState
  _ <- liftIO $ saveOpts opts
  M.halt
eventHandler ev = F.handleFormEvent ev

theMap =
  attrMap
    V.defAttr
    [ (E.editAttr, V.white `on` V.black),
      (E.editFocusedAttr, V.white `on` V.red),
      (F.focusedFormInputAttr, V.white `on` V.red)
    ]
