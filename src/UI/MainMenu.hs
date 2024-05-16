{-# LANGUAGE CPP, TemplateHaskell, OverloadedStrings #-}
module UI.MainMenu where

import Lens.Micro ((^.))
import Lens.Micro.Mtl
import Control.Monad (void)
import Control.Monad.State (modify)
import Data.Text(Text)
import qualified Graphics.Vty as V

import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Dialog as D
import qualified Brick.AttrMap as A
import qualified Data.Vector as Vec
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
  )
import Brick.Util (fg, on, bg)
import Brick.Widgets.Dialog (Dialog)
import Lens.Micro.TH (makeLenses)
import Brick (continueWithoutRedraw)

data Choice = PlayGame | HighScoresPage | QuitGame 
    deriving Show

data Name = Play | HighScores | Quit
    deriving (Eq, Show, Ord)


drawDialogUI :: D.Dialog Choice Name -> [Widget Name]
drawDialogUI d = pure ui
    where
        ui = D.renderDialog d $ C.hCenter $ padAll 1 $ txt "Choose an option"


appEvent :: T.BrickEvent Name e -> T.EventM Name (D.Dialog Choice Name) ()
appEvent (T.VtyEvent e) =
    case e of
        V.EvKey V.KEnter [] -> M.halt

        V.EvKey V.KEsc [] -> M.halt

        ev -> D.handleDialogEvent ev

appEvent _ = return ()
    

initialState :: D.Dialog Choice Name
initialState = D.dialog (Just $ txt " Main Menu ") (Just (Play, options)) 75
    where options = [ ("play", Play, PlayGame)
                    , ("high scores", HighScores, HighScoresPage)
                    , ("quit game", Quit, QuitGame)
                    ]

customAttr :: A.AttrName
customAttr = D.buttonSelectedAttr <> A.attrName "custom"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (D.dialogAttr,    V.white `on` V.black)
    , (D.buttonAttr,    V.white `on` V.red)
    , (D.buttonSelectedAttr,      bg V.blue)
    ]

mainMenuApp :: M.App (Dialog Choice Name) e Name
mainMenuApp = 
    M.App { M.appDraw = drawDialogUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return ()
          , M.appAttrMap = const theMap
          }

main :: IO ()
main = do 
    sel <- M.defaultMain mainMenuApp initialState
    putStrLn $ "Selection " <> show (D.dialogSelection sel)