{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module UI.Gameplay where

import Brick
import Brick.BChan (newBChan, writeBChan)
import Brick.Focus (FocusRing, focusGetCurrent, focusRingCursor)
import Brick.Forms (Form, (@@=))
import qualified Brick.Forms as F
import qualified Brick.Main as M
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Dialog (Dialog)
import qualified Brick.Widgets.Dialog as D
import qualified Brick.Widgets.List as L
----------
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import DB.Highscores as DBHS (Name, addScore, openDatabase, promptAddHighScore)
-------------

import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Vector as Vector
import Database.SQLite.Simple (Connection)
import GameLogic
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as V
----------------------
import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH
---------------------
import Linear.V2 (V2 (..))
---------------------
---------------------
import UI.Keybinds
import qualified Brick.Keybindings as K

-- | Marks passing of time.
--   Each delta is fed into the app.
data Tick = Tick

-- | The type of the cell
data Cell = Snake | Food | Empty

data MenuOptions = Resume | Restart | Quit | Yes | No | OpChar Int
  deriving (Show, Eq, Ord)

data GameplayState = GameplayState
  { _gameState :: GameState,
    _gameStateDialog :: Maybe (Dialog GameState MenuOptions),
    _highScoreDialogs :: HighScoreFormState,
    _tickNo :: Int
  }

data HighScoreFormState = HighScoreFormState
  { _hsDialog :: Maybe (Dialog HighScoreFormState MenuOptions),
    _hsForm :: Maybe (Form HighScoreForm () MenuOptions)
  }

data HighScoreForm = HighScoreForm {_cha1 :: Maybe Char, _cha2 :: Maybe Char, _cha3 :: Maybe Char}

makeLenses ''GameplayState
makeLenses ''HighScoreForm
makeLenses ''HighScoreFormState

gameplay :: IO ()
gameplay = do
  chan <- newBChan 10
  _ <- forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 100000 -- 10 ticks per second
  let initalVty = V.mkVty V.defaultConfig
  buildVty <- initalVty
  void $ customMain buildVty initalVty (Just chan) gameApp defState

gameApp :: M.App GameplayState Tick MenuOptions
gameApp =
  App
    { appDraw = drawUI,
      appChooseCursor = focusRingCursor F.formFocus . fromMaybe highScoreMkForm . _hsForm . _highScoreDialogs,
      appHandleEvent = eventHandler,
      appStartEvent = return (),
      appAttrMap = const theMap
    }

defState :: GameplayState
defState = GameplayState Restarting Nothing (HighScoreFormState Nothing Nothing) 0

dialogShower :: GameState -> Maybe (Dialog GameState MenuOptions)
dialogShower (Paused w) = Just $ D.dialog (Just (txt "PAUSE MENU")) (Just (Resume, options)) 40
  where
    options =
      [ ("Resume", Resume, Playing w),
        ("Restart", Restart, Restarting),
        ("Quit", Quit, ToMenu)
      ]
dialogShower (GameOver _) = Just $ D.dialog (Just (txt "REPLAY GAME?")) (Just (Yes, options)) 40
  where
    options =
      [ ("Yes", Yes, Restarting),
        ("No", No, ToMenu)
      ]
dialogShower (Starting w) = Just $ D.dialog (Just (txt "PRESS A DIRECTION OR ENTER TO START THE GAME")) (Just (Yes, options)) 70
  where
    options = [("GO", Yes, Playing w)]
dialogShower _ = Nothing

highScoreAskDialog :: World -> Dialog HighScoreFormState MenuOptions
highScoreAskDialog w =
  D.dialog
    ( Just
        ( txt $
            Text.concat
              [ "NEW HIGH SCORE OF ",
                Text.pack . show $ score w,
                " ACHIEVED.",
                " ADD TO LEADERBOARD?"
              ]
        )
    )
    (Just (Yes, options))
    120
  where
    options =
      [ ("Yes", Yes, HighScoreFormState Nothing (Just highScoreMkForm)),
        ("No", No, HighScoreFormState Nothing Nothing)
      ]

-- | Creates the form for the high score entry
highScoreMkForm :: Form HighScoreForm () MenuOptions
highScoreMkForm =
  F.setFormConcat hBox $
    F.newForm
      [ setup "1. " @@= F.listField fieldy cha1 listDrawElement 1 (OpChar 1),
        setup "2. " @@= F.listField fieldy cha2 listDrawElement 1 (OpChar 2),
        setup "3. " @@= F.listField fieldy cha3 listDrawElement 1 (OpChar 3)
      ]
      (HighScoreForm (Just 'A') (Just 'A') (Just 'A'))
  where
    setup s w = (withAttr L.listAttr $ vLimit 3 $ hLimit 3 $ padBottom Max $ txt s) <+> (hLimit 4 $ vLimit 3 w)
    fieldy = const $ Vector.iterateN 26 succ 'A'
    listDrawElement _ a = txt $ Text.singleton a

eventHandler :: BrickEvent MenuOptions Tick -> EventM MenuOptions GameplayState ()
eventHandler (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = M.halt
eventHandler ev = do
  gs <- use gameState
  case gs of
    Restarting -> do
      w <- liftIO $ initWorld defaultHeight defaultWidth
      tickNo .= 0 -- Reset the tick number to 0.
      gameState .= Starting w
    ToMenu -> M.halt
    Frozen _ -> do zoom gameState $ handleGameplayEvent' ev
    Playing _ -> do zoom gameState $ handleGameplayEvent' ev
                    tickNo %= (+1)
    Starting w -> do
      dia <- use gameStateDialog
      case dia of
        Nothing -> gameStateDialog .= dialogShower (Starting w)
        _ -> handleStartGameEvent ev
    NewHighScore w -> do
      conn <- liftIO $ openDatabase "highscores.db"
      hs <- liftIO $ promptAddHighScore conn (score w)
      if hs
        then do
          gameStateDialog .= Nothing
          highScoreDialogs .= HighScoreFormState (Just $ highScoreAskDialog w) Nothing
          gameState .= NewHighScorePrompt w conn
        else gameState .= GameOver w
    NewHighScorePrompt w conn -> do
      time <- liftIO (floor <$> getPOSIXTime)
      zoom highScoreDialogs $ handleHighScorePromptEvent ev conn w time
      hsd <- use highScoreDialogs
      case hsd of
        HighScoreFormState Nothing Nothing -> gameState .= GameOver w
        _ -> return ()
    p -> do
      dia <- use gameStateDialog
      case dia of
        Nothing -> gameStateDialog .= dialogShower p
        _ -> handleMenuEvent ev

handleHighScorePromptEvent :: BrickEvent MenuOptions Tick -> Connection -> World -> Int -> EventM MenuOptions HighScoreFormState ()
handleHighScorePromptEvent (VtyEvent (V.EvKey V.KEnter [])) conn w time = do
  st <- get
  case st of
    HighScoreFormState (Just dia) Nothing ->
      let result = maybe st snd (D.dialogSelection dia)
       in put result
    HighScoreFormState Nothing (Just form) ->
      do
        let HighScoreForm (Just c1) (Just c2) (Just c3) = F.formState form
        liftIO $ DBHS.addScore conn (Text.pack [c1, c2, c3]) (score w) time
        put (HighScoreFormState Nothing Nothing)
    _ -> return ()
handleHighScorePromptEvent (VtyEvent ev) _ _ _ = do
  st <- get
  case st of
    HighScoreFormState (Just _) Nothing -> zoom (hsDialog . _Just) $ D.handleDialogEvent ev
    HighScoreFormState Nothing (Just _) -> zoom (hsForm . _Just) $ F.handleFormEvent (VtyEvent ev)
    _ -> return ()
handleHighScorePromptEvent _ _ _ _ = return ()

-- | Handles changes in Gameplay
handleGameplayEvent' :: BrickEvent n1 Tick -> EventM n2 GameState ()
handleGameplayEvent' (AppEvent Tick) = modify stepGameState
handleGameplayEvent' (VtyEvent (V.EvKey k mods)) = do
  disp <- case gameplayDispatcher [] of
    Right disp -> return disp
    Left _ -> undefined
  _ <- K.handleKey disp k mods
  return ()
handleGameplayEvent' _ = return ()

-- | Handles controls for most dialogue menus
handleMenuEvent :: BrickEvent MenuOptions Tick -> EventM MenuOptions GameplayState ()
handleMenuEvent (VtyEvent (V.EvKey V.KEsc [])) = do
  gs <- use gameState
  case gs of
    Paused w -> gameState .= Playing w
    _ -> return ()
handleMenuEvent (VtyEvent (V.EvKey V.KEnter [])) = do
  gs <- use gameState
  dia <- use gameStateDialog
  case dia of
    Nothing -> return ()
    Just d -> do gameState .= maybe gs snd (D.dialogSelection d)
  gameStateDialog .= Nothing
handleMenuEvent (VtyEvent ev) = do
  zoom (gameStateDialog . _Just) $ D.handleDialogEvent ev
handleMenuEvent _ = return ()

-- | Starts the game in the direction the user specifies
handleStartGameEvent :: BrickEvent MenuOptions Tick -> EventM MenuOptions GameplayState ()
handleStartGameEvent ev@(VtyEvent (V.EvKey k _)) -- Start the game and move the Snake in the desired direction.
  | k `elem` [V.KUp, V.KDown, V.KLeft, V.KRight] =
      do
        handleMenuEvent (VtyEvent (V.EvKey V.KEnter []))
        zoom gameState $ handleGameplayEvent' ev
  | k == V.KEnter = handleMenuEvent ev
  | otherwise = return ()
handleStartGameEvent _ = return ()

-- | Controls the game and steps the game forward.
handleGameplayEvent :: BrickEvent n Tick -> EventM n GameState ()
handleGameplayEvent (AppEvent Tick) = modify stepGameState
handleGameplayEvent (VtyEvent (V.EvKey V.KUp [])) = modify (chDir U)
handleGameplayEvent (VtyEvent (V.EvKey V.KDown [])) = modify (chDir D)
handleGameplayEvent (VtyEvent (V.EvKey V.KLeft [])) = modify (chDir L)
handleGameplayEvent (VtyEvent (V.EvKey V.KRight [])) = modify (chDir R)
handleGameplayEvent (VtyEvent (V.EvKey (V.KChar 'p') [])) = do modify pauseToggle
handleGameplayEvent _ = pure ()

-- | Draws the overall UI of the game
drawUI :: GameplayState -> [Widget MenuOptions]
drawUI gps = [drawDebug gps] <> gpdia <> hsdia <> form <> (C.centerLayer <$> drawGS gs)
  where
    gs = gps ^. gameState
    gpdia = maybe [] (pure . C.centerLayer . (`D.renderDialog` emptyWidget)) (gps ^. gameStateDialog)
    hsdia = maybe [] (pure . C.centerLayer . (`D.renderDialog` emptyWidget)) (gps ^. highScoreDialogs . hsDialog)
    form = maybe [] (pure . C.center . F.renderForm) (gps ^. highScoreDialogs . hsForm)

-- | Draws the game depending on the state of the game
drawGS :: GameState -> [Widget MenuOptions]
drawGS Restarting = [emptyWidget]
drawGS ToMenu = [emptyWidget]
drawGS (GameOver gs) =
  [ padRight (Pad 2) (drawStats gs)
      <=> withAttr gameOverAttr (txt "GAME OVER")
      <+> drawGrid gs
  ]
drawGS (Paused gs) =
  [ vLimit defaultHeight $ C.centerLayer (padLeft (Pad 12) $ txt "PAUSED"),
    padRight (Pad 2) (drawStats gs)
      <+> drawGrid gs
  ]
drawGS gs = [padRight (Pad 2) (drawStats (getWorld gs)) <+> drawGrid (getWorld gs)]

drawDebug :: GameplayState -> Widget n
drawDebug gps = hLimit 11 $ vBox [ txt $ Text.pack $ show $ gps ^. tickNo ]

drawStats :: World -> Widget MenuOptions
drawStats w =
  hLimit 11 $
    vBox
      [ drawScore $ score w
      ]

drawScore :: Int -> Widget MenuOptions
drawScore n =
  withBorderStyle BS.unicodeBold $
    B.borderWithLabel (txt "Score") $
      C.hCenter $
        padAll 1 $
          txt $
            Text.pack $
              show n

drawGrid :: World -> Widget MenuOptions
drawGrid World {..} =
  withBorderStyle BS.unicodeBold $
    B.borderWithLabel (txt "Tobz-Snek") $
      vBox rows
  where
    rows = [hBox $ cellsInRow r | r <- [defaultHeight - 1, defaultHeight - 2 .. 0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0 .. defaultWidth - 1]]
    drawCoord = drawCell . cellAt
    cellAt c
      | c `elem` snake = Snake
      | c == food = Food
      | otherwise = Empty

drawCell :: Cell -> Widget MenuOptions
drawCell Snake = withAttr snakeAttr cell
drawCell Food = withAttr foodAttr cell
drawCell Empty = withAttr emptyAttr cell

snakeAttr, foodAttr, emptyAttr, gameOverAttr :: AttrName
snakeAttr = attrName "snakeAttr"
foodAttr = attrName "foodAttr"
emptyAttr = attrName "emptyAttr"
gameOverAttr = attrName "gameOver"

cell :: Widget MenuOptions
cell = txt "  "

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (snakeAttr, V.white `on` V.white),
      (foodAttr, V.red `on` V.red),
      (gameOverAttr, fg V.red `V.withStyle` V.bold),
      (D.buttonSelectedAttr, V.white `on` V.red),
      (D.buttonAttr, V.red `on` V.white),
      (D.dialogAttr, fg V.white),
      (L.listSelectedFocusedAttr, V.white `on` V.red),
      (L.listSelectedAttr, V.green `on` V.white),
      (L.listAttr, bg V.magenta)
    ]