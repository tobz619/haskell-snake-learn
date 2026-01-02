{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UI.Gameplay where

import Brick
import Brick.BChan (newBChan, writeBChan)
import Brick.Focus (focusRingCursor)
import Brick.Forms (Form, (@@=))
import qualified Brick.Forms as F
import qualified Brick.Keybindings as K
import qualified Brick.Main as M
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Dialog (Dialog)
import qualified Brick.Widgets.Dialog as D
import qualified Brick.Widgets.List as L
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void, join)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State.Class (MonadState)
import DB.Highscores as DBHS (addScore, promptAddHighScore, addScoreWithReplay, dbPath)
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Data.Word (Word16, Word64)
import Database.SQLite.Simple (Connection, withConnection)
import GameLogic
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as V
import Lens.Micro
import Lens.Micro.Mtl
import Linear.V2 (V2 (..))
import Logging.Logger
import System.IO
import System.Random
import UI.Keybinds (gameplayDispatcher)
import UI.Types
import DB.Client (runClientAppSTM, postScoreLeaderBoard, highScoreRequest)
import qualified Network.Wreq.Session as WreqS

altConfig :: [a]
altConfig = []

gameplay :: V.Vty  -> WreqS.Session -> IO V.Vty
gameplay vty session = do
  chan <- newBChan 64
  _ <- forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay (1_000_000 `div` 16) -- 16 ticks per second
  snd <$> customMainWithVty vty (V.mkVty V.defaultConfig) (Just chan) gameApp (defState {_sess = session})

gameApp :: M.App GameplayState Tick MenuOptions
gameApp =
  App
    { appDraw = drawUI,
      appChooseCursor = focusRingCursor F.formFocus . fromMaybe highScoreMkForm . _hsForm . _highScoreDialogs,
      appHandleEvent = eventHandler,
      appStartEvent = return (),
      appAttrMap = const theMap
    }

-- | Note that the session and seed are uninitialized.
defState :: GameplayState
defState = GameplayState Restarting Nothing (HighScoreFormState Nothing Nothing) 0 [] undefined undefined

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
    setup s w =
      withAttr L.listAttr (vLimit 3 $ hLimit 3 $ padBottom Max $ txt s)
        <+> hLimit 4 (vLimit 3 w)
    fieldy = const $ Vector.iterateN 26 succ 'A'
    listDrawElement _ a = txt $ Text.singleton a

genSeed :: (MonadState s m, MonadIO m) => m (SeedType, StdGen)
genSeed = do
  a <- randomRIO (minBound :: SeedType, maxBound :: SeedType)
  pure (a, mkStdGen a)

eventHandler ::
  BrickEvent MenuOptions Tick ->
  EventM MenuOptions GameplayState ()
eventHandler (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = M.halt
eventHandler ev = do
  gs <- use gameState
  sess' <- use sess 
  case gs of
    Restarting -> do
      vals@(genVal :: SeedType, g) <- genSeed
      let w = initWorld defaultHeight defaultWidth g
      -- sendGenToServer serverLocation g
      tickNo .= 0 -- Reset the tick number to 0.
      resetLog
      gameState .= Starting w
      gameSeed .= genVal
      logGameStart
    ToMenu -> M.halt
    Frozen _ -> do zoom gameState $ handleGameplayEvent' ev
    Playing w -> do
      zoom gameState $ handleGameplayEvent' ev
      tickNo %= (+ 1) -- advance the ticknumber by one
      -- mapM_ logEat (foodEaten w) -- Log if food is eaten
      logMove ev -- Log if a direction has been pressed
    Starting w -> do
      dia <- use gameStateDialog
      case dia of
        Nothing -> gameStateDialog .= dialogShower (Starting w)
        _ -> handleStartGameEvent ev 
    NewHighScore w -> do
      logGameEnd
      gps' <- get
      liftIO $ withFile "Seed-Events" WriteMode $ \h -> do
        hPrint h (gps' ^. gameSeed)
        hPrint h (gps' ^. gameLog)

      -- hs <- liftIO $ withConnection dbPath $ \conn -> promptAddHighScore (score w) conn
      hs <- liftIO $ highScoreRequest (score w) sess'
      if hs
        then do
          gameStateDialog .= Nothing
          highScoreDialogs .= HighScoreFormState (Just $ highScoreAskDialog w) Nothing
          gameState .= NewHighScorePrompt w
        else gameState .= GameOver w

    NewHighScorePrompt w -> do
      seed <- use gameSeed
      evList <- use gameLog
      zoom highScoreDialogs $ handleHighScorePromptEvent ev seed (score w) evList sess'

      hsd <- use highScoreDialogs
      case hsd of
        HighScoreFormState Nothing Nothing -> gameState .= GameOver w
        _ -> return ()
    p -> do
      dia <- use gameStateDialog
      maybe (gameStateDialog .= dialogShower p) (const $ handleMenuEvent ev) dia

handleHighScorePromptEvent :: BrickEvent MenuOptions Tick -> SeedType -> ScoreType -> EventList -> WreqS.Session -> EventM MenuOptions HighScoreFormState ()
handleHighScorePromptEvent (VtyEvent (V.EvKey V.KEnter [])) seed score evList sess = do
  st <- get
  case st of
    HighScoreFormState (Just dia) Nothing ->
      let result = maybe st snd (D.dialogSelection dia)
       in put result
    HighScoreFormState Nothing (Just form) -> do
      let HighScoreForm mC1 mC2 mC3 = F.formState form
          name = maybe Text.empty Text.pack (sequence [mC1, mC2, mC3])
      
      liftIO $ postScoreLeaderBoard name score seed evList sess
      put (HighScoreFormState Nothing Nothing)
    
    _ -> pure ()
handleHighScorePromptEvent (VtyEvent ev) _ _ _ _ = do
  st <- get
  case st of
    HighScoreFormState (Just _) Nothing -> zoom (hsDialog . _Just) $ D.handleDialogEvent ev
    HighScoreFormState Nothing (Just _) -> zoom (hsForm . _Just) $ F.handleFormEvent (VtyEvent ev)
    _ -> return ()
handleHighScorePromptEvent _ _ _ _ _ = return ()

-- | Handles changes in Gameplay and steps the game forward.
handleGameplayEvent' :: BrickEvent n1 Tick -> EventM n2 GameState ()
handleGameplayEvent' (AppEvent Tick) = modify stepGameState
handleGameplayEvent' (VtyEvent (V.EvKey k mods)) = do
  disp <- case gameplayDispatcher altConfig of
    Right disp -> return disp
    Left _ -> error "Somehow didn't get a dispatcher: this is impossible"
  void $ K.handleKey disp k mods
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
        logMove ev -- Log if a direction has been pressed
  | k == V.KEnter = handleStartGameEvent (VtyEvent (V.EvKey V.KUp []))
  | otherwise = return ()
handleStartGameEvent _ = return ()

-- | Draws the overall UI of the game
drawUI :: GameplayState -> [Widget MenuOptions]
drawUI gps =
  [drawDebug gps]
    <> gpdia
    <> hsdia
    <> form
    <> (C.centerLayer <$> drawGS gs)
  where
    gs = gps ^. gameState
    gpdia = maybe [] (pure . C.centerLayer . (`D.renderDialog` emptyWidget)) (gps ^. gameStateDialog)
    hsdia = maybe [] (pure . C.centerLayer . (`D.renderDialog` emptyWidget)) (gps ^. highScoreDialogs . hsDialog)
    form = maybe [] (pure . C.centerLayer . F.renderForm) (gps ^. highScoreDialogs . hsForm)

-- | Draws the game depending on the state of the game
drawGS :: GameState -> [Widget MenuOptions]
drawGS Restarting = [emptyWidget]
drawGS ToMenu = [emptyWidget]
drawGS (GameOver gs) =
  [ padRight (Pad 2) (drawStats gs)
      <=> padLeft (Pad 1) (withAttr gameOverAttr (txt "GAME OVER"))
      <+> drawGrid gs
  ]
drawGS (Paused gs) =
  [ vLimit defaultHeight $ C.centerLayer (padLeft (Pad 12) $ txt "PAUSED"),
    padRight (Pad 2) (drawStats gs)
      <+> drawGrid gs
  ]
drawGS gs = [padRight (Pad 2) (drawStats (getWorld gs)) <+> drawGrid (getWorld gs)]

drawDebug :: GameplayState -> Widget n
drawDebug gps = currentTick <=> currentLog
  where
    currentTick = hLimit 10 $ vBox [txt $ Text.pack $ show $ gps ^. tickNo]
    currentLog = vLimit 20 $ hLimit 45 $ vBox $ txt . Text.pack . show <$> take 20 (gps ^. gameLog)

drawStats :: World -> Widget n
drawStats w =
  hLimit 11 $
    vBox
      [ drawScore $ score w
      ]

drawScore :: (Show a, Num a) => a -> Widget n
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
      (foodAttr, bg V.red),
      (gameOverAttr, V.red `on` V.white `V.withStyle` V.bold),
      (D.buttonSelectedAttr, V.white `on` V.red),
      (D.buttonAttr, V.red `on` V.white),
      (D.dialogAttr, fg V.white),
      (L.listSelectedFocusedAttr, V.white `on` V.red),
      (L.listSelectedAttr, V.green `on` V.white),
      (L.listAttr, bg V.magenta)
    ]


