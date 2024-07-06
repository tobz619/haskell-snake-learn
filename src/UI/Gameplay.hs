{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module UI.Gameplay where

import Linear.V2 (V2(..))

import Lens.Micro
import Lens.Micro.TH
import Lens.Micro.Mtl

import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text

import GameLogic
import DB.Highscores (promptAddHighScore, openDatabase, Name)

import Brick
import qualified Brick.Main as M
import Brick.BChan
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Dialog(Dialog)
import qualified Brick.Widgets.Dialog as D
import Brick.Forms (Form, (@@=))
import qualified Brick.Forms as F

import qualified Graphics.Vty.CrossPlatform as V
import qualified Graphics.Vty as V



-- | Marks passing of time.  
--   Each delta is fed into the app.
data Tick = Tick

-- | The type of the cell
data Cell = Snake | Food | Empty

-- | The mode to of the page to activate different dialogs
data Mode = Game | Menu

data PauseMenuOptions = Resume | Restart | Quit | Yes | No
  deriving (Show, Eq, Ord)


data GameplayState = GameplayState { _gameState :: GameState
                                   , _runDialog :: Maybe (Dialog GameState PauseMenuOptions)
                                   , _runForm :: Maybe (Form HighScoreForm Tick Char)
                                   }

data HighScoreForm = HighScoreForm { _cha1 :: Char, _cha2 :: Char, _cha3 :: Char }

makeLenses ''GameplayState
makeLenses ''HighScoreForm



gameApp :: M.App GameplayState Tick PauseMenuOptions
gameApp = App { appDraw = drawUI
              , appChooseCursor = neverShowCursor
              , appHandleEvent = eventHandler
              , appStartEvent = return ()
              , appAttrMap = const theMap
              }

defState :: GameplayState
defState = GameplayState Restarting Nothing Nothing

dialogHandler :: GameState -> Maybe (Dialog GameState PauseMenuOptions)
dialogHandler (Paused w) = Just $ D.dialog (Just (txt "PAUSE MENU")) (Just (Resume, options)) 40
  where options = [ ("Resume", Resume, Playing w)
                  , ("Restart", Restart, Restarting)
                  , ("Quit", Quit, ToMenu)
                  ]

dialogHandler (NewHighScorePrompt w) = Just $ D.dialog (Just (txt $ Text.concat [ "NEW HIGH SCORE OF "
                                                                                , Text.pack . show $ score w
                                                                                ," ACHIEVED.", " ADD TO LEADERBOARD?"]
                                                             )
                                                       )
                                       (Just (Yes, options))
                                       120
  where options = [ ("Yes", Yes, NewHighScorePrompt w)
                  , ("No", No, GameOver w)
                  ]

dialogHandler (GameOver _) = Just $ D.dialog (Just (txt "REPLAY GAME?")) (Just (Yes, options)) 40
  where options = [ ("Yes", Yes, Restarting)
                  , ("No", No, ToMenu)
                  ]

dialogHandler (Starting w) = Just $ D.dialog (Just (txt "PRESS A DIRECTION OR ENTER TO START THE GAME")) (Just (Yes, options)) 70
  where options = [ ("GO", Yes, Playing w) ]

dialogHandler _ = Nothing


highScoreMkForm :: HighScoreForm -> Form HighScoreForm Tick Char
highScoreMkForm = F.newForm
  [ label "1" @@= F.radioField cha1 [(c,c, Text.pack [c]) | c <- ['a'..'z']]
  , label "2" @@= F.radioField cha2 [(c,c, Text.pack [c]) | c <- ['a'..'z']]
  , label "3" @@= F.radioField cha3 [(c,c, Text.pack [c]) | c <- ['a'..'z']]
  ]
  where label s w = padBottom (Pad 1) $
                      vLimit 1 (hLimit 15 $ str s <+> fill ' ') <+> w

gameplay :: IO ()
gameplay = do
  chan <- newBChan 10
  _ <- forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 100000
  let initalVty =  V.mkVty V.defaultConfig
  buildVty <- initalVty
  void $ customMain buildVty initalVty (Just chan) gameApp defState

eventHandler :: BrickEvent PauseMenuOptions Tick -> EventM PauseMenuOptions GameplayState ()
eventHandler (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = M.halt
eventHandler ev = do
  gs <- use gameState
  case gs of
    Restarting -> do w <- liftIO $ initWorld defaultHeight defaultWidth
                     gameState .= Starting w
    ToMenu -> M.halt
    Frozen _ -> do zoom gameState $ handleGameplayEvent ev
    Playing _ -> do zoom gameState $ handleGameplayEvent ev

    Starting w -> do
      dia <- use runDialog
      case dia of
        Nothing -> runDialog .= dialogHandler (Starting w)
        _ -> handleStartGameEvent ev

    NewHighScore w -> do
      conn <- liftIO $ openDatabase "highscores.db"
      hs <- liftIO $ promptAddHighScore conn (score w)
      if hs
        then gameState .= NewHighScorePrompt w
        else gameState .= GameOver w

    NewHighScorePrompt w -> do
      dia <- use runDialog
      case dia of
        Nothing -> runDialog .= dialogHandler (NewHighScorePrompt w)
        _ -> handleMenuEvent ev


    p -> do
      dia <- use runDialog
      case dia of
        Nothing -> runDialog .= dialogHandler p
        _ -> handleMenuEvent ev

handleMenuEvent :: BrickEvent PauseMenuOptions Tick -> EventM PauseMenuOptions GameplayState ()
handleMenuEvent (VtyEvent (V.EvKey V.KEsc [])) = do
  gs <- use gameState
  case gs of
    Paused w -> gameState .= Playing w
    _ -> return ()

handleMenuEvent (VtyEvent (V.EvKey V.KEnter [])) = do
  gs <- use gameState
  dia <- use runDialog
  case dia of
    Nothing -> return ()
    Just d -> do gameState .=  maybe gs snd (D.dialogSelection d)
  runDialog .= Nothing

handleMenuEvent (VtyEvent ev) = do
  zoom (runDialog ._Just) $ D.handleDialogEvent ev

handleMenuEvent _ = return ()

handleStartGameEvent :: BrickEvent PauseMenuOptions Tick -> EventM PauseMenuOptions GameplayState ()
handleStartGameEvent ev@(VtyEvent (V.EvKey k _))
  | k `elem` [V.KUp, V.KDown, V.KLeft, V.KRight] = do
    handleMenuEvent (VtyEvent (V.EvKey V.KEnter []))
    zoom gameState $ handleGameplayEvent ev
  | k == V.KEnter = handleMenuEvent ev
  | otherwise = return ()
handleStartGameEvent _ = return ()

handleGameplayEvent :: BrickEvent PauseMenuOptions Tick -> EventM PauseMenuOptions GameState ()
handleGameplayEvent (AppEvent Tick) = modify stepGameState
handleGameplayEvent (VtyEvent (V.EvKey V.KUp [])) = modify (chDir U)
handleGameplayEvent (VtyEvent (V.EvKey V.KDown [])) = modify (chDir D)
handleGameplayEvent (VtyEvent (V.EvKey V.KLeft [])) = modify (chDir L)
handleGameplayEvent (VtyEvent (V.EvKey V.KRight [])) = modify (chDir R)
handleGameplayEvent (VtyEvent (V.EvKey (V.KChar 'p') [])) = do modify pauseToggle
handleGameplayEvent _ = pure ()


drawUI :: GameplayState -> [Widget PauseMenuOptions]
drawUI gps = dia <> drawGS gs
          where gs = gps ^. gameState
                dia = maybe [] (pure . (`D.renderDialog` emptyWidget)) (gps ^. runDialog)


drawGS :: GameState -> [Widget PauseMenuOptions]
drawGS Restarting = [emptyWidget]
drawGS ToMenu = [emptyWidget]
drawGS (GameOver gs) = [C.hCenterLayer (padRight (Pad 2) (drawStats gs) <=> withAttr gameOverAttr (txt "GAME OVER"))
                       <+> drawGrid gs
                     ]
drawGS (Paused gs) = [ vLimit defaultHeight $ C.centerLayer (padLeft (Pad 12) $ txt "PAUSED")
                     , C.hCenterLayer (padRight (Pad 2) (drawStats gs))
                       <+> drawGrid gs
                     ]
drawGS gs = [C.hCenter $ padRight (Pad 2) (drawStats (getWorld gs)) <+> drawGrid (getWorld gs)]

drawStats :: World -> Widget PauseMenuOptions
drawStats w = hLimit 11 $
                vBox [ drawScore $ score w
                     ]

drawScore :: Int -> Widget PauseMenuOptions
drawScore n = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (txt "Score")
  $ C.hCenter
  $ padAll 1
  $ txt $ Text.pack $ show n


drawGrid :: World -> Widget PauseMenuOptions
drawGrid  World{..} = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (txt "Tobz-Snek")
  $ vBox rows
  where rows = [hBox $ cellsInRow r | r <- [defaultHeight-1, defaultHeight-2 .. 0]]
        cellsInRow y = [drawCoord (V2 x y) | x <- [0 .. defaultWidth-1]]
        drawCoord = drawCell . cellAt
        cellAt c
          | c `elem` snake = Snake
          | c == food = Food
          | otherwise = Empty

drawCell :: Cell -> Widget PauseMenuOptions
drawCell Snake = withAttr snakeAttr cw
drawCell Food  = withAttr foodAttr cw
drawCell Empty = withAttr emptyAttr cw

snakeAttr, foodAttr, emptyAttr, gameOverAttr :: AttrName
snakeAttr = attrName "snakeAttr"
foodAttr  = attrName "foodAttr"
emptyAttr = attrName "emptyAttr"
gameOverAttr = attrName "gameOver"

cw :: Widget PauseMenuOptions
cw = txt "  "

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (snakeAttr, V.white `on` V.white)
  , (foodAttr, V.red `on` V.red)
  , (gameOverAttr, fg V.red `V.withStyle` V.bold)
  , (D.buttonSelectedAttr, V.white `on` V.red)
  , (D.buttonAttr, V.red `on` V.white)
  , (D.dialogAttr, fg V.white)
  ]