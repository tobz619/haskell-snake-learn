{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module GameLogic where

import Control.Monad.State
  ( MonadState (get, put),
    State,
    execState,
  )
import Data.Sequence.NonEmpty (NESeq ((:<||), (:||>)), (|>))
import qualified Data.Sequence.NonEmpty as S
import Database.SQLite.Simple (Connection)
import Lens.Micro (over)
import Linear.V2 (V2 (..), _x, _y)
import System.Random (Random (..), StdGen)
import Data.Word (Word16)

data GameState
  = Playing {getWorld :: World}
  | Paused {getWorld :: World}
  | Frozen {getWorld :: World}
  | GameOver {getWorld :: World}
  | ToMenu
  | Starting {getWorld :: World}
  | Restarting
  | NewHighScore {getWorld :: World}
  | NewHighScorePrompt {getWorld :: World}

instance Show GameState where
  show (Playing w) = show w
  show (NewHighScore w) = show w 
  show (GameOver w) = show w
  show _ = ""

instance Eq GameState where
  Playing w0 == Playing w1 = w0 == w1
  Paused w0 == Paused w1 = w0 == w1
  Frozen w0 == Frozen w1 = w0 == w1
  GameOver w0 == GameOver w1 = w0 == w1
  ToMenu == ToMenu = True
  Starting w0 == Starting w1 = w0 == w1
  Restarting == Restarting = True
  NewHighScore w0 == NewHighScore w1 = w0 == w1
  NewHighScorePrompt w0 == NewHighScorePrompt w1 = w0 == w1
  _ == _ = False

type Coord = V2 Int

type Snake = NESeq Coord

type ScoreType = Word16

data Stream a = a :| Stream a
  deriving (Show)

data Direction = U | D | L | R | NoDir
  deriving (Eq, Show)

data World = World
  { snake :: Snake,
    dir :: Direction,
    food :: Coord,
    foods :: Stream Coord,
    score :: ScoreType,
    foodEaten :: Maybe Coord
  }

instance Show World where
  show w = 
    "Snake: " <> show (snake w) <> "\n" <>
    "Current food: " <> show (food w) <> "\n" <>
    "Score: " <> show (score w) 


instance Eq World where
  (==) :: World -> World -> Bool
  world0 == world1 =
    and
      [ snake world0 == snake world1,
        dir world0 == dir world1,
        food world0 == food world1,
        score world0 == score world1
      ]

defaultHeight, defaultWidth :: Int
defaultHeight = 20
defaultWidth = 20

-- | Step forward in time
stepGameState :: GameState -> GameState
stepGameState (Playing w) = advanceWorld w
stepGameState (Frozen w) = advanceWorld w
stepGameState gs = gs

advanceWorld :: World -> GameState
advanceWorld w@World {..} = case die w of
  Playing _ ->
    let newSnake =
          if eatFood food snake
            then
              execState nextFood $
                w
                  { snake = (moveSnake dir . growSnake) snake,
                    score = score + 1,
                    foodEaten = Just food
                  }
            else w {snake = moveSnake dir snake, foodEaten = Nothing}
     in Playing newSnake
  gs -> gs

stepReverseGameState :: GameState -> GameState
stepReverseGameState (Playing w) = reverseGameState w
stepReverseGameState (Frozen w) = reverseGameState w
stepReverseGameState gs = gs

reverseGameState :: World -> GameState
reverseGameState w@World {..} = case die w of
  Playing _ -> 
    let newSnake = w {snake = reverseSnake dir snake}
    in Playing newSnake
  gs -> gs

-- | Die if the current head position is disallowed
die :: World -> GameState
die g@World {..} =
  let outOfBounds ((V2 x y) :<|| _) = or [x <= 0, x >= defaultWidth, y <= 0, y >= defaultHeight]
      illegal (hd :<|| snakeTail)
        | hd `elem` snakeTail || outOfBounds snake = NewHighScore g
        | otherwise = Playing g
   in illegal snake

eatFood :: Coord -> Snake -> Bool
eatFood c (s :<|| _) = c == s

growSnake, shrinkSnake :: Snake -> Snake
growSnake snake@(_ :||> t) = snake |> t
shrinkSnake snake = S.withNonEmpty snake id (S.init snake)

moveSnake, reverseSnake :: Direction -> Snake -> Snake
moveSnake U s@(hd :<|| _) = over _y (+ 1) hd :<|| S.init s
moveSnake D s@(hd :<|| _) = over _y (subtract 1) hd :<|| S.init s
moveSnake R s@(hd :<|| _) = over _x (+ 1) hd :<|| S.init s
moveSnake L s@(hd :<|| _) = over _x (subtract 1) hd :<|| S.init s
moveSnake NoDir s = s

reverseSnake U s@(_ :||> lst) = S.tail s :||> over _y (+ 1) lst
reverseSnake D s@(_ :||> lst) = S.tail s :||> over _y (subtract 1) lst
reverseSnake L s@(_ :||> lst) = S.tail s :||> over _x (+ 1) lst
reverseSnake R s@(_ :||> lst) = S.tail s :||> over _x (subtract 1) lst
-- reverseSnake U s = moveSnake D (S.reverse s)
-- reverseSnake D s = moveSnake U (S.reverse s)
-- reverseSnake R s = moveSnake L (S.reverse s)
-- reverseSnake L s = moveSnake R (S.reverse s)
reverseSnake NoDir s = s

nextFood :: State World ()
nextFood = do
  g@World {snake, foods} <- get
  let (f :| fs) = foods
  if f `elem` snake
    then put (g {foods = fs}) >> nextFood
    else put $ g {food = f, foods = fs}

prevFood :: Coord -> State World ()
prevFood old = do
  g@World {food, foods} <- get
  put $ g {
    food = old,
    foods = food :| foods
  }


chDir :: Direction -> GameState -> GameState
chDir to (Playing World {..}) = Frozen $ World {dir = turnDir dir to, ..}
chDir _ s = s

turnDir :: Direction -> Direction -> Direction
turnDir NoDir to = to
turnDir from to
  | opposite from == to || to == from = from
  | otherwise = to
  where
    opposite U = D
    opposite L = R
    opposite R = L
    opposite D = U
    opposite NoDir = to

pauseToggle :: GameState -> GameState
pauseToggle st = case st of
  Playing g -> Paused g
  Paused g -> Playing g
  s -> s

initWorld :: Int -> Int -> StdGen -> World
initWorld height width initGen =
  let (f :| fs) = fromList $ randomRs (V2 1 1, V2 (height - 1) (width - 1)) initGen
      xm = defaultWidth `div` 2
      ym = defaultHeight `div` 2
      w =
        World
          { snake = S.singleton (V2 xm ym),
            food = f,
            foods = fs,
            score = 0,
            dir = NoDir,
            foodEaten = Nothing
          }
   in execState nextFood w
  where
    fromList = foldr (:|) (error "Stream must be infinite!")
