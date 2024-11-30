import Prelude hiding (map, filter, zipWith, uncurry, foldr, zip)
import Data.Char

-- For each of the functions, add comments where necessary to what the type is
-- Follow the evaluation tree to calculate the correct result

-- ============================================================================

-- foldr is given as an example of what is expected

foldr :: 
  (a -> b -> b) -- A function that takes a value of type `a` and
                -- a value of type `b` and returns a b
  -> b -- A starting value of type `b`
  -> [a] -- A list of `a`'s which we are going to process
  -> b -- A resulting value of type `b` that we return after processing the list
foldr _   b [] = b 
foldr abb b (a:as) = abb a (foldr abb b as)

-- foldr (+) 0 ( [3,2,1] :: [Int]) -- has type of list of Ints
-- (+) 3 (foldr (+) 0 [2,1])
-- (+) 3 ((+) 2 (foldr (+) 0 [1]))
-- (+) 3 ((+) 2 ((+) 1 (foldr (+) 0 []))
-- (+) 3 ((+) 2 ((+) 1 0))
-- (+) 3 ((+) 2 1)
-- (+) 3 3
-- 6 :: Int

-- ============================================================================


zipWith :: 
  (a -> b -> c) -- ??
  -> [a] -- ??
  -> [b] -- ??
  -> [c] -- A list of `c`'s is returned
zipWith _   []    _       = []
zipWith _   _     []      = []
zipWith abc (a:as) (b:bs) = abc a b : zipWith abc as bs

-- zipWith (++) ["Hello", "World"] [" Michael", " Domination", " Mouse"]
-- (++) "Hello" " Michael" : zipWith (++) ["World"] [ " Domination", " Mouse"]
-- "Hello Michael" : zipWith (++) ["World"] [ " Domination", " Mouse"]
-- "Hello Michael" : (++) "World" " Domination" : zipWith (++) [] [" Mouse"]
-- "Hello Mciahel" : "World Domination" : []
-- ["Hello Michael","World Domination"]

-- ============================================================================

map :: 
  (i -> o) -- A function that takes a value of type `i` and returns
           -- a value of type `o`
  -> [i] -- A list of `i`'s
  -> [o] -- Returns a list of o's
map _ []      = []
map ab (a:as) = ab a : func ab as

-- map (\ (_:y:_) -> y ) ["bye", "car" "asphalt", "island"]
-- ??

-- ============================================================================

filter :: 
  (a -> Bool) -- ??
  -> [a] -- ??
  -> [a] --??
filter _ [] = []
filter p (el:els)
  | p el = el : filter p els
  | otherwise = filter p els

-- filter (\x -> length x >= 3) ["which", "is", "three"]
-- ??

-- ============================================================================


zip :: 
  [a] -- ??
  -> [b] -- ??
  -> [(a,b)] -- ??
zip [] _ = []
zip _ [] = []
zip (a:as) (b:bs) = (a,b) : zip as bs

-- zip [1,2] ["wow", "this", "is", "a", "really", "long", "list"]
-- ??

-- ============================================================================

data Car = MkCar String Int

uncurry :: 
  (a -> b -> c) -- ??
  -> (a ,b) -- ??
  -> c -- ??
uncurry abc (a,b) = abc a b

-- uncurry MkCar ("Porsche", 1991)
-- ??

