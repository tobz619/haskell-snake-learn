{-# LANGUAGE BangPatterns #-}

module SnakeApp where

import UI.Gameplay (gameplay)
import UI.HighscoreScreens (highScores)
import UI.MainMenu (Choice (..), runMainMenu)
import qualified Graphics.Vty.CrossPlatform as V
import qualified Graphics.Vty as V
import qualified Control.Exception as E

main :: IO ()
main = do
  vty <- V.mkVty V.defaultConfig
  E.onException (loop vty) (V.shutdown vty) 

  where loop v = do
          !(choice, v') <- runMainMenu v
          case choice of
            Play -> gameplay v' >>= loop 
            HighScores -> highScores v' >>= loop
            Quit -> V.shutdown v'

