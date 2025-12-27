{-# LANGUAGE BangPatterns #-}

module SnakeApp where

import UI.Gameplay (gameplay)
import UI.HighscoreScreens (highScores)
import UI.MainMenu (Choice (..), runMainMenu)

main :: IO ()
main = do
  pure ()
  !choice <- runMainMenu
  case choice of
    Play -> gameplay >> main
    HighScores -> highScores >> main
    Quit -> pure ()

