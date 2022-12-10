{-# LANGUAGE LambdaCase #-}

module Day10 (day10) where
import Control.Monad.State (State, put, get, runState)
import Data.Map (Map, insert, elems)
import Data.List (foldl')

day10 :: String -> String
day10 input =
  let xs = (map parseInstr $ lines input) :: [Instr Integer]

      part1 = sum
        . elems
        . snd
        $ foldl' (\(a, s) instr -> runState (runInstr' a 1 instr) s) (Cpu 1 1, mempty) xs

      part2 = unlines
        . map reverse
        . reverse
        . snd
        $ foldl' (\(a, s) instr -> runState (runInstr a 1 instr) s) (Cpu 1 1, mempty) xs

  in show part1 <> "\n" <> part2

data Cpu a = Cpu Integer a

data Instr a = Noop | Addx a
  deriving Show

parseInstr :: Read a => String -> Instr a
parseInstr xs = case words xs of
  ["noop"] -> Noop
  ["addx", ys] -> Addx $ read ys
  _ -> error "bad parse"

runInstr' :: Cpu Integer -> Int -> Instr Integer -> State (Map Integer Integer) (Cpu Integer)
runInstr' (Cpu c x) tick instr = do
  m <- get

  if c `elem` [20, 60, 100, 140, 180, 220]
    then put $ insert c (x * c) m
    else put m

  let cpu = case (instr, tick) of
        (Noop, 1) -> Cpu (c+1) x
        (Addx _, 1) -> Cpu (c+1) x
        (Addx y, 2) -> Cpu (c+1) (x+y)
        _ -> error "bad clock input"

  case (instr, tick) of
    (Addx _, 1) -> runInstr' cpu 2 instr
    _ -> return cpu

runInstr :: Cpu Integer -> Int -> Instr Integer -> State [String] (Cpu Integer)
runInstr (Cpu c x) tick instr = do
  ss <- get

  let i = (c - 1) `mod` 40
  let ss' = if i == 0 then [] : ss else ss

  put $ (getPixel ((c - 1) `mod` 40) [x-1,x,x+1] : head ss') : tail ss'

  let cpu = case (instr, tick) of
        (Noop, 1) -> Cpu (c+1) x
        (Addx _, 1) -> Cpu (c+1) x
        (Addx y, 2) -> Cpu (c+1) (x+y)
        _ -> error "bad clock input"

  case (instr, tick) of
    (Addx _, 1) -> runInstr cpu 2 instr
    _ -> return cpu

getPixel :: Integer -> [Integer] -> Char
getPixel p sprite =
  if p `elem` sprite
    then '#'
    else '.'




