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
      onl <- view online <$> getOpts
      t <- forkIO . forever $ do
        _ <- join $ swapMVar <$> pure mv <*> (if onl then heartbeatRequest s else pure False)
        threadDelay (5 * 10 ^ 6)
      !(choice, v') <- runMainMenu v
      case choice of
        Play -> E.finally (gameplay v' mv s) (killThread t) >>= loop mv s
        HighScores -> E.finally (highScores mv v' s) (killThread t) >>= loop mv s
        Options -> E.finally (options v') (killThread t) >>= loop mv s
        Quit -> V.shutdown v'
