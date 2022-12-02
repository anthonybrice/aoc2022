module Day1 (day1) where

import Data.List (sortBy)

type Elf = Integer

day1 :: String -> String
day1 input =
  let xs = parseInputToElves input
  in show (part1 xs) <> "\n" <> show (part2 xs)

parseInputToElves :: String -> [Elf]
parseInputToElves input = map (sum . map read) . splitWhen (== "") $ lines input where
  splitWhen _ [] = []
  splitWhen cond xs = foldr (\y acc -> 
    if cond y 
    then [] : acc
    else (y : head acc) : tail acc
    ) [[last xs]] $ init xs

part1 :: [Elf] -> Elf
part1 = maximum

part2 :: [Elf] -> Elf
part2 xs = sum . take 3 $ sortBy (flip compare) xs
