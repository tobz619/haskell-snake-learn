module SnakeApp where

import UI.Gameplay (gameplay)
import UI.HighscoreScreens (highScores)
import UI.MainMenu (Choice (..), runMainMenu)

newtype AppState r = AppState {runApp :: IO r}

main :: IO ()
main = do
  choice <- runMainMenu
  case choice of
    Play -> gameplay >> main
    HighScores -> highScores >> main
    Quit -> return ()
