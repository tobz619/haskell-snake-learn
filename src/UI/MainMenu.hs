{-# LANGUAGE CPP #-}
module UI.MainMenu where

import Lens.Micro ((^.))
import Lens.Micro.Mtl
import Control.Monad (void)
import Control.Monad.State (modify)
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import Data.Text(Text)
import qualified Data.Text as Text
import qualified Graphics.Vty as V

import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Center as C
import qualified Brick.AttrMap as A
import qualified Data.Vector as Vec
import Brick.Types
  ( Widget
  )
import Brick.Widgets.Core
  ( (<+>)
  , str
  , vLimit
  , hLimit
  , vBox
  , withAttr
  )
import Brick.Util (fg, on)

drawUI ::  L.List () String -> [Widget ()]
drawUI l = [ui]
    where
        box = B.border $
              hLimit 25 $
              vLimit 12 $
              L.renderList listDrawElement True l
        ui = C.vCenter $ vBox [ C.hCenter box
                              , str " "
                              , C.hCenter $ str "Press Enter to select an option."
                              , C.hCenter $ str "Press Esc to exit."
                              ]

appEvent :: T.BrickEvent () e -> T.EventM () (L.List () String) ()
appEvent (T.VtyEvent e) =
    case e of
        V.EvKey V.KEnter [] -> do
            return ()

        V.EvKey V.KEsc [] -> M.halt

        ev -> L.handleListEvent ev
    

listDrawElement :: Bool -> String -> Widget ()
listDrawElement sel a =
    let selStr s = if sel
                   then withAttr customAttr $ str s
                   else str s
    in C.hCenter . selStr $ a

initialState :: L.List () String
initialState = L.list () (Vec.fromList ["Play", "High scores", "Quit game"]) 3

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> A.attrName "custom"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (L.listAttr,            V.white `on` V.black)
    , (L.listSelectedAttr,    V.white `on` V.red)
    , (customAttr,            fg V.blue)
    ]

mainMenuApp :: M.App (L.List () String) e ()
mainMenuApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return ()
          , M.appAttrMap = const theMap
          }

main :: IO ()
main = void $ M.defaultMain mainMenuApp initialState