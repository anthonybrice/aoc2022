{-# LANGUAGE LambdaCase #-}
module Day9 (day9, Direction(..)) where

import Prelude hiding (Right, Left)
import Data.List (foldl', nub)

day9 :: String -> String
day9 input =
  let xs = map parseMove $ lines input
      part1 = length
        . nub
        . map snd
        $ foldl' (\acc m -> uncurry (trail m) (head acc) <> acc) [((0,0),(0,0))] xs

      part2 = length
        . nub
        . map last
        $ foldl' (\acc m -> ropeTrail m (head acc) <> acc) [replicate 10 (0,0)] xs

  in show part1 <> "\n" <> show part2

data Move = Move Direction Int

data Direction = Up | Down | Left | Right
  deriving Eq

-- | Rope position
type RP = (Integer, Integer)

type Rope = [RP]

parseMove :: String -> Move
parseMove (x:' ':y) = Move (f x) (read y) where
  f = \case
    'R' -> Right
    'U' -> Up
    'L' -> Left
    'D' -> Down
    _ -> error ""
parseMove _ = error ""

ropeTrail :: Move -> Rope -> [Rope]
ropeTrail _ [] = []
ropeTrail (Move d i) rs = foldr (\d' acc -> singleMove' d' (head acc) : acc) [rs] singleMoves
  where
    singleMoves = if i > 1 then replicate i d else [d]

singleMove' :: Direction -> Rope -> Rope
singleMove' _ [] = []
singleMove' d (h : rs) =
  let h' = move h d
  in reverse $ foldl' (\acc r -> follow (head acc) r : acc) [h'] rs

trail :: Move -> RP -> RP -> [(RP, RP)]
trail (Move d i) rp1 rp2 = foldr (\d' acc -> uncurry (singleMove d') (head acc) : acc) [(rp1, rp2)] singleMoves
  where
    singleMoves = if i > 1 then replicate i d else [d]

singleMove :: Direction -> RP -> RP -> (RP, RP)
singleMove d h@(x, y) t@(x', y')
  | x == x' + 1 && y == y' && d == Right = (move h d, h)
  | x == x' - 1 && y == y' && d == Left = (move h d, h)
  | x == x' && y == y' + 1 && d == Up = (move h d, h)
  | x == x' && y == y' - 1 && d == Down = (move h d, h)
  | x == x' - 1 && y == y' + 1 && (d == Up || d == Left) = (move h d, h)
  | x == x' + 1 && y == y' + 1 && (d == Up || d == Right) = (move h d, h)
  | x == x' + 1 && y == y' - 1 && (d == Down || d == Right) = (move h d, h)
  | x == x' - 1 && y == y' - 1 && (d == Down || d == Left) = (move h d, h)
  | otherwise = (move h d, t)

move :: RP -> Direction -> RP
move (x,y) d' = case d' of
  Right -> (x+1,y)
  Up -> (x,y+1)
  Left -> (x-1,y)
  Down -> (x,y-1)

-- | Given a moved head and the tail's current position, calculate the tail's next position
follow :: RP -> RP -> RP
follow (x, y) t@(x', y')
  | x == x' - 2 && y == y' + 2 = (x' - 1, y' + 1)
  | x == x' - 1 && y == y' + 2 = (x' - 1, y' + 1)
  | x == x' && y == y' + 2 = (x', y' + 1)
  | x == x' + 1 && y == y' + 2 = (x' + 1, y' + 1)
  | x == x' + 2 && y == y' + 2 = (x' + 1, y' + 1)
  | x == x' - 2 && y == y' + 1 = (x' - 1, y' + 1)
  | x == x' + 2 && y == y' + 1 = (x' + 1, y' + 1)
  | x == x' - 2 && y == y' = (x' - 1, y')
  | x == x' + 2 && y == y' = (x' + 1, y')
  | x == x' - 2 && y == y' - 1 = (x' - 1, y' - 1)
  | x == x' + 2 && y == y' - 1 = (x' + 1, y' - 1)
  | x == x' - 2 && y == y' - 2 = (x' - 1, y' - 1)
  | x == x' - 1 && y == y' - 2 = (x' - 1, y' -  1)
  | x == x' && y == y' - 2 = (x', y' - 1)
  | x == x' + 1 && y == y' - 2 = (x' + 1, y' - 1)
  | x == x' + 2 && y == y' - 2 = (x' +1, y' - 1)
  | otherwise = t