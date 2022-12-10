{-# LANGUAGE LambdaCase #-}

module Day10 (day10) where
import Control.Monad.State (State, put, get, runState)
import Data.Map (Map, insert, elems)
import Data.List (foldl')
import Control.Monad

day10 :: String -> String
day10 input =
  let xs = (map parseInstr $ lines input) :: [Instr Integer]

      part1 = sum
        . elems
        . snd
        $ foldl' (\(a, s) instr -> runState (runInstr' a 1 instr) s) (Cpu 1 1, mempty) xs

      part2 = unlines
        . f
        . reverse
        . snd
        $ foldl' (\(a, s) instr -> runState (runInstr a 1 instr) s) (Cpu 1 1, mempty) xs

  in show part1 <> "\n" <> part2

data Cpu a = Cpu Integer a

data Instr a = Noop | Addx a
  deriving Show

f :: String -> [String]
f xs =
  let (s1, r1) = splitAt 40 xs
      (s2, r2) = splitAt 40 r1
      (s3, r3) = splitAt 40 r2
      (s4, r4) = splitAt 40 r3
      (s5, r5) = splitAt 40 r4
    in [s1, s2, s3, s4, s5, r5]

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

runInstr :: Cpu Integer -> Int -> Instr Integer -> State String (Cpu Integer)
runInstr (Cpu c x) tick instr = do
  s <- get

  put $ getPixel ((c - 1) `mod` 40) [x-1,x,x+1] : s

  let cpu@(Cpu c' x') = case (instr, tick) of
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




