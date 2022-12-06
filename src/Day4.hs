module Day4 (day4) where

import Control.Arrow ((***), second)
import Control.Monad (join)

day4 :: String -> String
day4 input = do
  let xs = lines input
      part1 = length $ filter (/= False) $ map (uncurry isContained . parseLine) xs
      part2 = length $ filter (/= False) $ map (uncurry isOverlapping . parseLine) xs
  show part1 <> "\n" <> show part2

type Range = (Integer, Integer)

parseLine :: String -> (Range, Range)
parseLine xs = join (***) parseRange . second tail $ break (== ',') xs

parseRange :: String -> Range
parseRange xs = join (***) read . second tail $ break (== '-') xs

isContained :: Range -> Range -> Bool
isContained (s1, e1) (s2, e2) =
  (s1 <= s2 && e1 >= e2) || (s2 <= s1 && e2 >= e1)

isOverlapping :: Range -> Range -> Bool
isOverlapping (s1, e1) (s2, e2) =
  (s1 <= s2 && s2 <= e1) || (s2 <= s1 && s1 <= e2)