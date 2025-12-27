-- {-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}

module UI.MainMenu where

import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import Brick.Types (Widget)
import qualified Brick.Types as T
import Brick.Util (fg, on)
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core (
    clickable,
    emptyWidget,
    padAll,
    reportExtent,
    txt,
 )
import qualified Brick.Widgets.Dialog as D
import Data.Maybe (fromMaybe)
import qualified Graphics.Vty as V
import Lens.Micro.Mtl
import Lens.Micro.TH (makeLenses)

data Choice = Play | HighScores | Quit
    deriving (Eq, Show, Ord)

data DialogState = DialogState
    { _dialog :: D.Dialog Choice Choice
    , _dialogChoice :: Maybe Choice
    }

makeLenses ''DialogState

drawDialogUI :: D.Dialog Choice Choice -> [Widget Choice]
drawDialogUI d = pure ui
  where
    ui = D.renderDialog d . C.hCenter . padAll 1 $ focus
    focus =
        maybe
            emptyWidget
            (\n -> reportExtent n . (clickable <*> widgetTitle) $ n)
            $ D.getDialogFocus d

widgetTitle :: Choice -> Widget n
widgetTitle Play = txt "Play the game"
widgetTitle HighScores = txt "View highscores"
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
        [ ("play", Play, Play)
        , ("high scores", HighScores, HighScores)
        , ("quit game", Quit, Quit)
        ]

theMap :: A.AttrMap
theMap =
    A.attrMap
        V.defAttr
        [ (D.dialogAttr, fg V.white)
        , (D.buttonAttr, V.red `on` V.white)
        , (D.buttonSelectedAttr, V.white `on` V.red)
        ]

mainMenuApp :: M.App DialogState e Choice
mainMenuApp =
    M.App
        { M.appDraw = drawDialogUI . view dialog
        , M.appChooseCursor = M.neverShowCursor
        , M.appHandleEvent = appEvent
        , M.appStartEvent = return ()
        , M.appAttrMap = const theMap
        }

runMainMenu :: IO Choice
runMainMenu = do
    !res <- _dialogChoice <$> M.defaultMain mainMenuApp initialState
    pure (fromMaybe Quit res)
