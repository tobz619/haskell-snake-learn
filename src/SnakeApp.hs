module SnakeApp where

import UI.MainMenu ( Choice(..), runMainMenu )
import UI.Gameplay ( gameplay )
import UI.HighscoreScreens (highScores)

import qualified Brick.Main as M

newtype AppState r =  AppState { runApp :: IO r}

main :: IO ()
main = do
  choice <- runMainMenu
  case choice of
    Play -> gameplay >> main
    HighScores -> highScores >> main
    Quit -> return ()