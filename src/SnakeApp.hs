{-# LANGUAGE BangPatterns #-}

module SnakeApp where

import UI.Gameplay (gameplay)
import UI.HighscoreScreens (highScores)
import UI.MainMenu (Choice (..), runMainMenu)
import qualified Graphics.Vty.CrossPlatform as V
import qualified Graphics.Vty as V
import qualified Control.Exception as E
import qualified Network.Wreq.Session as WreqS

main :: IO ()
main = do
  vty <- V.mkVty V.defaultConfig
  sess <- WreqS.newSession
  E.onException (loop sess vty) (V.shutdown vty) 

  where loop s v  = do
          !(choice, v') <- runMainMenu v
          case choice of
            Play -> gameplay v' s >>= loop s
            HighScores -> highScores v' s >>= loop s
            Quit -> V.shutdown v'

