{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
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
import qualified Brick.Widgets.Border.Style as BBS
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Dialog (Dialog)
import qualified Brick.Widgets.Dialog as D
import qualified Brick.Widgets.List as L
import Control.Concurrent (MVar, forkIO, threadDelay)
import Control.Concurrent.MVar (readMVar)
import Control.Monad (forever, join, void, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State.Class (MonadState)
import DB.Client (highScoreRequest, postScoreLeaderBoard, heartbeatRequest)
import DB.Highscores (addScoreWithReplay, dbPath, promptAddHighScore)
import DB.Send (evListEncoder)
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Vector as Vector
import Database.SQLite.Simple (withConnection)
import GameLogic
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as V
import Lens.Micro ((^.), _Just)
import Lens.Micro.Mtl (use, (%=), (.=))
import Linear.V2 (V2 (..))
import Logging.Logger
  ( logGameEnd,
    logGameStart,
    logMove,
    resetLog,
  )
import qualified Network.Wreq.Session as WreqS
import Options.Options (getOpts, online)
import System.IO (IOMode (WriteMode), hPrint, withFile)
import System.Random (StdGen, mkStdGen, randomRIO)
import UI.Keybinds (gameplayDispatcher)
import UI.Types
import qualified Options.Options as Options

altConfig :: [a]
altConfig = []

gameplay :: V.Vty -> WreqS.Session -> IO V.Vty
gameplay vty session = do
  chan <- newBChan 64
  userOpts <- getOpts
  _ <- forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay (1_000_000 `div` 16) -- 16 ticks per second
  v <- snd <$> customMainWithVty vty (V.mkVty V.defaultConfig) (Just chan) (gameApp session) (defState userOpts)
  () <$ Options.saveOpts userOpts
  pure v

gameApp :: WreqS.Session -> App GameplayState Tick MenuOptions
gameApp sess =
  App
    { appDraw = drawUI,
      appChooseCursor = focusRingCursor F.formFocus . fromMaybe highScoreMkForm . _hsForm . _highScoreDialogs,
      appHandleEvent = eventHandler sess,
      appStartEvent = return (),
      appAttrMap = const theMap
    }

-- | Note that the session and seed are uninitialized.
defState :: Options.Options -> GameplayState
defState = GameplayState Restarting Nothing (HighScoreFormState Nothing Nothing) 0 [] undefined StartingMode

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

highScoreAskDialog :: World -> Bool -> Dialog HighScoreFormState MenuOptions
highScoreAskDialog w isOnline =
  D.dialog
    ( Just
        ( txt $
            Text.concat
              [ "NEW HIGH SCORE OF ",
                Text.pack . show $ score w,
                " ACHIEVED.",
                " ADD TO " <> (if isOnline then "ONLINE " else "LOCAL ") <> "LEADERBOARD?"
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

eventHandler :: WreqS.Session -> BrickEvent MenuOptions Tick -> EventM MenuOptions GameplayState ()
eventHandler _ (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = M.halt
eventHandler sess' ev = do
  gs <- use gameState
  case gs of
    Restarting -> do
      (genVal :: SeedType, g) <- genSeed
      let w = initWorld defaultHeight defaultWidth g
      -- sendGenToServer serverLocation g
      tickNo .= 0 -- Reset the tick number to 0.
      resetLog
      gameState .= Starting w
      gameSeed .= Just genVal
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


      allowsOnline <- (^. online) <$> use opts 
      connected <- liftA2 (&&) (pure allowsOnline) (liftIO $ heartbeatRequest sess')
      hs <-
        if not connected
          then liftIO $ withConnection dbPath $ \conn -> promptAddHighScore (score w) conn
          else liftIO $ highScoreRequest (score w) sess'
      if hs
        then do
          gameStateDialog .= Nothing
          highScoreDialogs .= HighScoreFormState (Just $ highScoreAskDialog w connected) Nothing
          gameState .= NewHighScorePrompt w
        else gameState .= GameOver w
    NewHighScorePrompt w -> do
      seed <- fromMaybe 0 <$> use gameSeed
      evList <- use gameLog
      useOnline <- (^. online) <$> use opts
      zoom highScoreDialogs $ handleHighScorePromptEvent sess' useOnline seed (score w) evList sess' ev

      hsd <- use highScoreDialogs
      case hsd of
        HighScoreFormState Nothing Nothing -> gameState .= GameOver w
        _ -> return ()
    p -> do
      dia <- use gameStateDialog
      maybe (gameStateDialog .= dialogShower p) (const $ handleMenuEvent ev) dia

handleHighScorePromptEvent :: WreqS.Session -> Bool -> SeedType -> ScoreType -> EventList -> WreqS.Session -> BrickEvent MenuOptions Tick -> EventM MenuOptions HighScoreFormState ()
handleHighScorePromptEvent sess setOnline seed score evList sessio (VtyEvent (V.EvKey V.KEnter [])) = do
  dia <- use hsDialog
  form <- use hsForm
  put =<< maybe get (\d -> maybe <$> get <*> pure snd <*> pure (D.dialogSelection d)) dia
  let chars = Text.pack <$> (F.formState <$> form >>= \(HighScoreForm a b c) -> sequence [a, b, c])
  con <- pure setOnline >>= \b -> if b then liftIO (heartbeatRequest sess) else pure b  
  t <- liftIO . fmap floor $ getPOSIXTime
  maybe
    (pure ())
    (\cs -> liftIO $ withConnection dbPath $ addScoreWithReplay cs score t seed (evListEncoder evList))
    chars
  when con $
    maybe
      (pure ())
      ( \cs ->
          (liftIO $ postScoreLeaderBoard cs score seed evList sessio)
            >> put (HighScoreFormState Nothing Nothing)
      )
      chars
handleHighScorePromptEvent _ _ _ _ _ _ (VtyEvent ev) = do
  form <- use hsForm
  dia <- use hsDialog
  maybe (pure ()) (\_ -> zoom (hsDialog . _Just) $ D.handleDialogEvent ev) dia
  maybe (pure ()) (\_ -> zoom (hsForm . _Just) $ F.handleFormEvent (VtyEvent ev)) form
handleHighScorePromptEvent _ _ _ _ _ _ _ = return ()

-- | Handles changes in Gameplay and steps the game forward.
handleGameplayEvent' :: BrickEvent n1 Tick -> EventM n2 GameState ()
handleGameplayEvent' (AppEvent Tick) = modify stepGameState
handleGameplayEvent' (VtyEvent (V.EvKey k mods)) = do
  disp <-
    either
      (error "Somehow didn't get a dispatcher: this is impossible")
      pure
      (gameplayDispatcher altConfig)
  () <$ K.handleKey disp k mods
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
  [ drawDebug gps,
    gpdia,
    hsdia,
    form,
    C.centerLayer (drawGS gs)
  ]
  where
    gs = gps ^. gameState
    gpdia = maybe emptyWidget ((`D.renderDialog` emptyWidget)) (gps ^. gameStateDialog)
    hsdia = maybe emptyWidget ((`D.renderDialog` emptyWidget)) (gps ^. highScoreDialogs . hsDialog)
    form = maybe emptyWidget (C.centerLayer . F.renderForm) (gps ^. highScoreDialogs . hsForm)

-- | Draws the game depending on the state of the game
drawGS :: GameState -> Widget MenuOptions
drawGS Restarting = emptyWidget
drawGS ToMenu = emptyWidget
drawGS (GameOver gs) =
  ( padRight (Pad 2) (drawStats gs)
      <=> padLeft (Pad 1) (withAttr gameOverAttr (txt "GAME OVER"))
  )
    <+> drawGrid gs
drawGS (Paused gs) =
  ( padRight (Pad 2) (drawStats gs)
      <=> (padLeft (Pad 1) . withAttr pausedAttr $ txt "PAUSED")
  )
    <+> drawGrid gs
drawGS gs = padRight (Pad 2) (drawStats (getWorld gs)) <+> drawGrid (getWorld gs)

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
  withBorderStyle BBS.unicodeBold $
    B.borderWithLabel (txt "Score") $
      C.hCenter $
        padAll 1 $
          txt $
            Text.pack $
              show n

drawGrid :: World -> Widget MenuOptions
drawGrid World {..} =
  withBorderStyle BBS.unicodeBold $
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

snakeAttr, foodAttr, emptyAttr, gameOverAttr, pausedAttr :: AttrName
snakeAttr = attrName "snakeAttr"
foodAttr = attrName "foodAttr"
emptyAttr = attrName "emptyAttr"
gameOverAttr = attrName "gameOver"
pausedAttr = attrName "paused"

cell :: Widget MenuOptions
cell = txt "  "

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (snakeAttr, V.white `on` V.white),
      (foodAttr, bg V.red),
      (gameOverAttr, V.red `on` V.white `V.withStyle` V.bold),
      (pausedAttr, V.white `on` V.blue `V.withStyle` V.bold),
      (D.buttonSelectedAttr, V.white `on` V.red),
      (D.buttonAttr, V.red `on` V.white),
      (D.dialogAttr, fg V.white),
      (L.listSelectedFocusedAttr, V.white `on` V.red),
      (L.listSelectedAttr, V.green `on` V.white),
      (L.listAttr, bg V.magenta)
    ]
