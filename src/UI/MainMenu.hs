{-# LANGUAGE CPP, TemplateHaskell, OverloadedStrings #-}
module UI.MainMenu where

import Lens.Micro ((^.), set)
import Lens.Micro.Mtl
import Lens.Micro.TH(makeLenses)

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
import Brick (clickable)


data Choice = Play | HighScores | Quit
    deriving (Eq, Show, Ord)

data DialogState = DialogState { dialog :: D.Dialog Choice Choice
                               , dialogChoice :: Maybe Choice
                               }


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


appEvent :: T.BrickEvent Choice e -> T.EventM Choice DialogState ()
appEvent (T.MouseDown _ V.BLeft _  _) = appEvent (T.VtyEvent (V.EvKey V.KEnter []))
appEvent (T.VtyEvent e) =
    case e of
        V.EvKey V.KEnter [] -> do
            d <- T.gets dialog
            let res = fst <$> D.dialogSelection d
            T.modify $ \st -> st {dialogChoice = res}
            M.halt


        V.EvKey V.KEsc [] -> M.halt

        ev -> do d <- T.gets dialog
                 res <- T.nestEventM' d (D.handleDialogEvent ev)
                 T.modify $ \st -> st { dialog = res }

appEvent _ = return ()

defaultDialogChoice :: Dialog Choice Choice -> DialogState
defaultDialogChoice d = DialogState d Nothing

initialState :: DialogState
initialState = defaultDialogChoice d
    where d = D.dialog (Just $ txt " Main Menu ") (Just (Play, options)) 125
          options = [ ("play", Play, Play)
                    , ("high scores", HighScores, HighScores)
                    , ("quit game", Quit, Quit)
                    ]

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (D.dialogAttr,    V.white `on` V.black)
    , (D.buttonAttr,    V.red `on` V.white)
    , (D.buttonSelectedAttr,      bg V.red  )
    ]

mainMenuApp :: M.App DialogState e Choice
mainMenuApp =
    M.App { M.appDraw = drawDialogUI . dialog
          , M.appChooseCursor = M.neverShowCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return ()
          , M.appAttrMap = const theMap
          }

runMainMenu :: IO (Maybe Choice)
runMainMenu = do
    res <- dialogChoice <$> M.defaultMain mainMenuApp initialState
    putStrLn $ "You chose: " ++ maybe "INVALID" show res
    return res