{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day5 (day5) where

import Day5.Parser (parseInput)
import Day5.Types (Stack, Move, Move(Move))
import Text.ParserCombinators.ReadP (readP_to_S)
import Data.List (foldl')

day5 :: String -> String
day5 input =
  let ((stacks, moves), "") = last $ readP_to_S parseInput input
      part1 = map head $ unload9000 stacks moves
      part2 = map head $ unload9001 stacks moves
  in part1 <> "\n" <> part2


unload9000 :: [Stack] -> [Move] -> [Stack]
unload9000 = foldl' runMove9000

unload9001 :: [Stack] -> [Move] -> [Stack]
unload9001 = foldl' runMove9001

runMove9000 :: [Stack] -> Move -> [Stack]
runMove9000 xs (Move count from to) =
  let crates = reverse . take count $ xs !! (from - 1)
      newToStack = crates <> (xs !! (to - 1))
      newFromStack = drop count $ xs !! (from - 1)
  in replaceAt newFromStack (from-1) $ replaceAt newToStack (to-1) xs

runMove9001 :: [Stack] -> Move -> [Stack]
runMove9001 xs (Move count from to) =
  let crates = take count $ xs !! (from - 1)
      newToStack = crates <> (xs !! (to - 1))
      newFromStack = drop count $ xs !! (from - 1)
  in replaceAt newFromStack (from-1) $ replaceAt newToStack (to-1) xs

replaceAt :: a -> Int -> [a] -> [a]
replaceAt x i xs =
  let (prefix, suffix) = splitAt i xs
  in prefix <> [x] <> tail suffix
