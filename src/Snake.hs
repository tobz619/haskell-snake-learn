{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module Snake where

import Data.Sequence.NonEmpty (NESeq ((:||>), (:<||)), (|>))
import qualified Data.Sequence.NonEmpty as S
import Linear.V2 (V2(..), _x, _y)
import System.Random (Random(..), newStdGen)

import Control.Monad.State
import Control.Monad.Reader
import Lens.Micro (over)

data GameState = Playing {getWorld :: World}
               | Paused {getWorld :: World}
               | Frozen {getWorld :: World}
               | GameOver {getWorld :: World}
               deriving (Show)

data World = World
  {
    snake :: Snake
  , dir :: Direction
  , food :: Coord
  , foods :: Stream Coord
  , score :: Int
  } deriving Show

type Coord = V2 Int
type Snake = NESeq Coord

data Stream a = a :| Stream a
  deriving Show

data Direction = U | D | L | R
  deriving (Eq, Show)

height, width :: Int
height = 20
width = 20


-- | Step forward in time
stepGameState :: GameState -> GameState
stepGameState (Playing w) = advanceWorld w
stepGameState (Frozen w) = advanceWorld w
stepGameState gs = gs

advanceWorld :: World -> GameState
advanceWorld w@World{..} = do
  case die w of
    Playing _ -> let newSnake = if eatFood food snake
                                 then execState nextFood $ 
                                          w { snake = 
                                              (moveSnake dir . growSnake) snake,
                                              score = score + 1
                                            }
                                 else w { snake = moveSnake dir snake }
                 in Playing newSnake
    gs -> gs

-- | Die if the current head position is disallowed
die :: World -> GameState
die g@World{..} = 
    let outOfBounds ((V2 x y ):<|| _) = or [x <= 0, x >= width, y <= 0, y >= height]
        illegal (hd :<|| snakeTail)
          | hd `elem` snakeTail || outOfBounds snake = GameOver g
          | otherwise = Playing g

  in illegal snake

eatFood :: Coord -> Snake -> Bool
eatFood c (s :<|| _) = c == s

growSnake :: Snake -> Snake
growSnake snake@(_ :||> t) = snake |> t

moveSnake :: Direction -> Snake -> Snake
moveSnake U s@(hd :<|| _) = over _y (+1) hd :<|| S.init s
moveSnake D s@(hd :<|| _) = over _y (subtract 1) hd :<|| S.init s
moveSnake R s@(hd :<|| _) = over _x (+1) hd :<|| S.init s
moveSnake L s@(hd :<|| _) = over _x (subtract 1) hd :<|| S.init s

nextFood :: State World ()
nextFood = do
  g@World{ snake, foods } <- get
  let (f :| fs) = foods
  if f `elem` snake
    then put (g {foods = fs}) >> nextFood
    else put $ g {food = f, foods = fs }

chDir :: Direction -> GameState -> GameState
chDir to (Playing World{..}) = Frozen $ World { dir = turnDir dir to, .. }
chDir _ s = s

turnDir :: Direction -> Direction -> Direction
turnDir from to
  | opposite from == to || to == from = from
  | otherwise = to
    where opposite U = D
          opposite L = R
          opposite R = L
          opposite D = U

pauseToggle :: GameState -> GameState
pauseToggle st = case st of
                  Playing g -> Paused g
                  Paused g -> Playing g
                  s -> s

initWorld :: IO World
initWorld = do
  (f :| fs) <-
    fromList . randomRs (V2 1 1, V2 (width - 1) (height - 1)) <$> newStdGen
  let xm = width `div` 2
      ym = height `div` 2
      g  = World
        { snake  = S.singleton (V2 xm ym)
        , food   = f
        , foods  = fs
        , score  = 0
        , dir    = U
        }
  return $ execState nextFood g
    where fromList = foldr (:|) (error "Stream must be infinite!")