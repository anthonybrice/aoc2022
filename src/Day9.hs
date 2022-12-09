{-# LANGUAGE LambdaCase #-}
module Day9 (day9, Direction(..)) where

import Prelude hiding (Right, Left)
import Data.List (foldl', nub)

day9 :: String -> String
day9 input =
  let xs = map parseMove $ lines input
      part1 = length
        . nub
        . map last
        $ foldl' (\acc m -> ropeTrail m (head acc) <> acc) [replicate 2 (0,0)] xs

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

move :: RP -> Direction -> RP
move (x,y) d' = case d' of
  Right -> (x+1,y)
  Up -> (x,y+1)
  Left -> (x-1,y)
  Down -> (x,y-1)

touching :: RP -> RP -> Bool
touching (x, y) (x', y') = abs (x - x') <= 1 && abs (y - y') <= 1

-- | Given a moved head and the tail's current position, calculate the tail's next position
follow :: RP -> RP -> RP
follow h@(x, y) t@(x', y') =
  if touching h t then t else (x' + signum (x - x'), y' + signum (y - y'))