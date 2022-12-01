module Day1 (day1) where

import Data.List.Split (splitWhen)
import Data.List (sortBy)

type Elf = Integer

day1 :: IO ()
day1 = do
  input <- readFile "input/day1"
  let xs = parseInputToElves input

  print $ part1 xs
  print $ part2 xs

parseInputToElves :: String -> [Elf]
parseInputToElves input = map (sum . map read) . splitWhen (== "") $ lines input

part1 :: [Elf] -> Elf
part1 = maximum

part2 :: [Elf] -> Elf
part2 xs = sum . take 3 $ sortBy (flip compare) xs

