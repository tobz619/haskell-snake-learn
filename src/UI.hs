{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module UI where

import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Data.Maybe

import GameLogic

import Brick
import Brick.BChan
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty.CrossPlatform as V
import Linear.V2 (V2(..))
import Lens.Micro ((^.))
import qualified Graphics.Vty as V
import Lens.Micro.TH (makeLenses)

-- | Marks passing of time.  
--   Each delta is fed into the app.
data Tick = Tick

-- | Named resource
type Name = ()

data Cell = Snake | Food | Empty

app :: App GameState Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return ()
          , appAttrMap = const theMap
          }

main :: IO ()
main = do
  chan <- newBChan 10
  _ <- forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 100000
  w <- initWorld
  -- void $ defaultMain app g
  let initalVty =  V.mkVty V.defaultConfig
  buildVty <- initalVty
  void $ customMain buildVty initalVty (Just chan) app (Paused w)

handleEvent :: BrickEvent Name Tick
            -> EventM Name GameState ()
handleEvent (AppEvent Tick) = modify stepGameState
handleEvent (VtyEvent (V.EvKey V.KUp [])) = modify (chDir U)
handleEvent (VtyEvent (V.EvKey V.KDown [])) = modify (chDir D)
handleEvent (VtyEvent (V.EvKey V.KLeft [])) = modify (chDir L)
handleEvent (VtyEvent (V.EvKey V.KRight [])) = modify (chDir R)
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent (VtyEvent (V.EvKey (V.KChar 'p') [])) = modify pauseToggle
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
handleEvent _ = pure ()


drawUI :: GameState -> [Widget Name]
drawUI (GameOver gs) = [C.hCenterLayer (padRight (Pad 2) (drawStats gs) <=> str "GAME OVER")
                       <+> drawGrid gs
                     ]
drawUI (Paused gs) = [C.hCenterLayer (padRight (Pad 2) (drawStats gs) <=> str "PAUSED")
                       <+> drawGrid gs
                     ]
drawUI gs = [C.hCenter $ padRight (Pad 2) (drawStats (getWorld gs)) <+> drawGrid (getWorld gs)]

drawStats :: World -> Widget Name
drawStats w = hLimit 11 $
                        vBox [ drawScore $ score w
                        ]

drawScore :: Int -> Widget Name
drawScore n = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Score")
  $ C.hCenter
  $ padAll 1
  $ str $ show n

-- drawGameOver :: GameState -> Widget Name
-- drawGameOver st = case st of
--                   GameOver -> withAttr gameOverAttr $ C.hCenter $ str "GameState OVER"
--                   _ -> emptyWidget

drawGrid :: World -> Widget Name
drawGrid World{..} = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Tobz-Snek")
  $ vBox rows
  where rows = [hBox $ cellsInRow r | r <- [height-1, height-2 .. 0]]
        cellsInRow y = [drawCoord (V2 x y) | x <- [0 .. width-1]]
        drawCoord = drawCell . cellAt
        cellAt c
          | c `elem` snake = Snake
          | c == food = Food
          | otherwise = Empty

drawCell :: Cell -> Widget Name
drawCell Snake = withAttr snakeAttr cw
drawCell Food  = withAttr foodAttr cw
drawCell Empty = withAttr emptyAttr cw

snakeAttr, foodAttr, emptyAttr, gameOverAttr :: AttrName
snakeAttr = attrName "snakeAttr"
foodAttr  = attrName "foodAttr"
emptyAttr = attrName "emptyAttr"
gameOverAttr = attrName "gameOver"

cw :: Widget Name
cw = str "  "

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (snakeAttr, V.white `on` V.white)
  , (foodAttr, V.red `on` V.red)
  , (gameOverAttr, fg V.red `V.withStyle` V.bold)
  ]