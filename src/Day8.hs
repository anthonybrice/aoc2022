module Day8 (day8) where
import Data.Map (Map, insert)
import qualified Data.Map as Map
import Data.Char (digitToInt)
import Data.Maybe (fromJust, isNothing)
import Data.List (find)

day8 :: String -> String
day8 input =
  let xs' = lines input
      xs = map readRow xs'
      h = length xs
      l = length $ head xs
      grid' = buildGrid xs
      grid = Grid grid' (h, l)
      pairs = [(x, y) | x <- [0..h-1], y <- [0..l-1]]
      part1 = length . filter (== True) $ map (isVisible grid) pairs
      part2 = maximum $ map (scenicScore grid) pairs
  in show part1 <> "\n" <> show part2

data Grid = Grid (Map (Int, Int) Int) (Int, Int)
  deriving (Show, Read)

readRow :: String -> [Int]
readRow = map digitToInt

buildGrid :: [[Int]] -> Map (Int, Int) Int
buildGrid xss =
  let l = length xss
      g = mempty
  in snd $ foldr f (l - 1, g) xss where
    f xs (rowIndex, g) = (rowIndex - 1, buildRow g rowIndex xs)

buildRow :: Map (Int, Int) Int -> Int -> [Int] -> Map (Int, Int) Int
buildRow g rowIndex xs =
  let l = length xs
  in snd $ foldr f (l - 1, g) xs where
    f x (columnIndex, g') = (columnIndex - 1, insert (rowIndex, columnIndex) x g')

isVisible :: Grid -> (Int, Int) -> Bool
isVisible _ (0, _) = True
isVisible _ (_, 0) = True
isVisible (Grid g (h, l)) (x, y)
  | x == h - 1 = True
  | y == l - 1 = True
  | otherwise = top || right || bottom || left where
    top =
      let xs = map (\x' -> fromJust $ Map.lookup (x', y) g) [0..x]
      in isNothing $ find (\a -> a >= last xs) (init xs)
    right =
      let xs = map (\y' -> fromJust $ Map.lookup (x, y') g) [y..l-1]
      in isNothing $ find (\a -> a >= head xs) (tail xs)
    bottom =
      let xs = map (\x' -> fromJust $ Map.lookup (x', y) g) [x..h-1]
      in isNothing $ find (\a -> a >= head xs) (tail xs)
    left =
      let xs = map (\y' -> fromJust $ Map.lookup (x, y') g) [0..y]
      in isNothing $ find (\a -> a >= last xs) (init xs)

scenicScore :: Grid -> (Int, Int) -> Int
scenicScore _ (0, _) = 0
scenicScore _ (_, 0) = 0
scenicScore (Grid g (h, l)) (x, y)
  | x == h - 1 = 0
  | y == l - 1 = 0
  | otherwise = top * right * bottom * left where
    top =
      let xs = map (\x' -> fromJust $ Map.lookup (x', y) g) [0..x]
      in length $ takeUntil (>= last xs) (tail $ reverse xs)
    right =
      let xs = map (\y' -> fromJust $ Map.lookup (x, y') g) [y..l-1]
      in length $ takeUntil (>= head xs) (tail xs)
    bottom =
      let xs = map (\x' -> fromJust $ Map.lookup (x', y) g) [x..h-1]
      in length $ takeUntil (>= head xs) (tail xs)
    left =
      let xs = map (\y' -> fromJust $ Map.lookup (x, y') g) [0..y]
      in length $ takeUntil (>= last xs) (tail $ reverse xs)

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil p = foldr (\x ys -> x : if p x then [] else ys) []
