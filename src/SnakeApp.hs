{-# LANGUAGE BangPatterns #-}

module SnakeApp where

import Control.Concurrent (killThread, threadDelay)
import Control.Concurrent.MVar (newMVar, swapMVar)
import qualified Control.Exception as E
import Control.Monad (forever, join, when)
import DB.Client (heartbeatRequest)
import GHC.Conc (forkIO)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as V
import Lens.Micro.Mtl (view)
import qualified Network.Wreq.Session as WreqS
import Options.Options (getOpts, online)
import UI.Gameplay (gameplay)
import UI.HighscoreScreens (highScores)
import UI.MainMenu (Choice (..), runMainMenu)
import UI.Options (options)

main :: IO ()
main = do
  vty <- V.mkVty V.defaultConfig
  sess <- WreqS.newSession
  mHB <- newMVar False
  E.onException (loop mHB sess vty) (V.shutdown vty)
  where
    loop mv s v = do
      !(choice, v') <- runMainMenu v
      case choice of
        Play -> gameplay v' mv s >>= loop mv s
        HighScores -> highScores v' s >>= loop mv s
        Options -> options v' >>= loop mv s
        Quit -> V.shutdown v'
