module SnakeApp where

import UI.MainMenu
import UI.Gameplay ( gameplay )

import qualified Brick.Main as M

newtype AppState r =  AppState { runApp :: IO r}

main :: IO ()
main = do
  choice <- runMainMenu
  case choice of
    Play -> gameplay
    HighScores -> return ()
    Quit -> return ()