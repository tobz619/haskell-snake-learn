{-# LANGUAGE CPP, TemplateHaskell, OverloadedStrings #-}
module UI.MainMenu where

import Lens.Micro ((^.))
import Lens.Micro.Mtl
import qualified Graphics.Vty as V

import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Dialog as D
import qualified Brick.AttrMap as A
import Brick.Types
  ( Widget
  )
import Brick.Widgets.Core
  ( (<+>)
  , txt
  , padAll
  , vLimit
  , hLimit
  , vBox
  , withAttr
  , emptyWidget, reportExtent
  )
import Brick.Util (fg, on, bg)
import Brick.Widgets.Dialog (Dialog)
import Data.Maybe (fromMaybe)
import Brick (clickable)
import qualified Graphics.Vty as T



data Choice = Play | HighScores | Quit
    deriving (Eq, Show, Ord)


drawDialogUI :: D.Dialog Choice Choice -> [Widget Choice]
drawDialogUI d = pure ui
    where
        ui =  D.renderDialog d . C.hCenter . padAll 1 $ focus
        focus = 
            maybe 
            emptyWidget 
            (\n -> reportExtent n . (clickable <*> nameWidget) $ n) 
            $ D.getDialogFocus d

nameWidget :: Choice -> Widget n
nameWidget Play = txt "Play the game"
nameWidget HighScores = txt "View highscores"
nameWidget Quit = txt "Quit the game" 

appEvent :: T.BrickEvent Choice e -> T.EventM Choice (D.Dialog Choice Choice) ()
appEvent (T.MouseDown _ V.BLeft _  _) = appEvent (T.VtyEvent (V.EvKey V.KEnter []))
appEvent (T.VtyEvent e) =
    case e of
        V.EvKey V.KEnter [] -> do
            dialog <- T.get
            let (n,_) = fromMaybe (Quit, Quit) $ D.dialogSelection dialog
            M.halt

        V.EvKey V.KEsc [] -> M.halt

        ev -> D.handleDialogEvent ev

appEvent _ = return ()
    

initialState :: D.Dialog Choice Choice
initialState = D.dialog (Just $ txt " Main Menu ") (Just (Play, options)) 125
    where options = [ ("play", Play, Play)
                    , ("high scores", HighScores, HighScores)
                    , ("quit game", Quit, Quit)
                    ]

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (D.dialogAttr,    V.white `on` V.black)
    , (D.buttonAttr,    V.red `on` V.white)
    , (D.buttonSelectedAttr,      bg V.red  )
    ]

mainMenuApp :: M.App (Dialog Choice Choice) e Choice
mainMenuApp = 
    M.App { M.appDraw = drawDialogUI
          , M.appChooseCursor = M.neverShowCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return ()
          , M.appAttrMap = const theMap
          }

main :: IO ()
main = do 
    sel <- M.defaultMain mainMenuApp initialState
    putStrLn $ "Selection " <> show (D.dialogSelection sel)