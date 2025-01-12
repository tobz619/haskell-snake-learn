import Data.Function
import Data.List

main = putStrLn "Hi Tobi!"

data Car = MkCar {make :: String, model :: String}
    deriving (Show)

mkAudis = map (MkCar "Audi") ["R8", "TT", "A1"]

data Board = MkBoard {deck :: [Int], columns :: [Column]}

type Column = [(Int, Bool)]

instance Show Board where
    show (MkBoard d cs) =
        let
            remDeck = "Deck size: " ++ (show (length d))
            showColumns = labelCols cs
         in
            unlines [remDeck, "", showColumns]

nums = [101 .. 152]

mkBoard = go [1 .. 7] []
  where
    go [] acc rest = MkBoard rest (reverse acc)
    go (n : ns) cols ds =
        let (col, rest) = ((\x -> (x, False)) <$> take n ds, drop n ds)
         in go ns (col : cols) rest

unsnoc = foldr (\x -> Just . maybe ([], x) (\ ~(a, b) -> (x : a, b))) Nothing

labelCols cols =
    let
        addSpace = (map . map) (++ " ")
        enclose i = ("[" ++ show i ++ "]")
        showCard (c, b)
            | b = show c
            | otherwise = "???"
        revealFirst xs = maybe xs (\(ys, (c, _)) -> ys ++ [(c, True)]) (unsnoc xs)
        labelled = transpose . addSpace . fastPad $ zipWith (:) (map enclose [0 ..]) ((map . map) showCard (map revealFirst cols))
        fastPad xss = let maxLength = foldr (max . length) 0 xss in map (\xs -> xs ++ replicate (maxLength - length xs) "   ") xss
     in
        unlines $ concat <$> labelled

test = show $ (\b -> b{columns = (columns b)}) $ mkBoard nums
