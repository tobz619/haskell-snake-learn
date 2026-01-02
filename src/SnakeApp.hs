{-# LANGUAGE BangPatterns #-}

module SnakeApp where

import UI.Gameplay (gameplay)
import UI.HighscoreScreens (highScores)
import UI.MainMenu (Choice (..), runMainMenu)
import qualified Graphics.Vty.CrossPlatform as V
import qualified Graphics.Vty as V
import qualified Control.Exception as E
import qualified Network.Wreq.Session as WreqS
import Control.Concurrent.MVar (newMVar, swapMVar)
import Control.Concurrent (threadDelay)
import DB.Client (heartbeatRequest)
import GHC.Conc (forkIO)
import Control.Monad (forever, join)

main :: IO ()
main = do
  vty <- V.mkVty V.defaultConfig
  sess <- WreqS.newSession
  mHB <- newMVar False
  _ <- forkIO . forever $ do
    _ <- join $ swapMVar <$> pure mHB <*> heartbeatRequest sess
    threadDelay (1*10^6)
  E.onException (loop mHB sess vty) (V.shutdown vty) 

  where loop mv s v  = do
          !(choice, v') <- runMainMenu v
          case choice of
            Play -> gameplay v' s >>= loop mv s
            HighScores -> highScores mv v' s >>= loop mv s
            Quit -> V.shutdown v'

