module Lib
    ( aoc2022
    ) where

import System.Environment (getArgs)

import Day1 (day1)
import Day2 (day2)

aoc2022 :: IO ()
aoc2022 = do
  args <- getArgs
  case head args of
    "day1" -> do
      input <- readFile "input/day1"
      putStrLn $ day1 input
    "day2" -> do
      input <- readFile "input/day2"
      putStrLn $ day2 input
    _ -> print "Usage: aoc2022 dayN"