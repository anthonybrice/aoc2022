{-# LANGUAGE LambdaCase #-}
module Day9 (day9, singleMove, Direction(..)) where

import Prelude hiding (Right, Left)
import Data.List (foldl', nub)

day9 :: String -> String
day9 input =
  let xs = map parseMove $ lines input
      part1 = length . nub . map snd $ foldl' (\acc m -> trail (head acc) m <> acc) [((0,0),(0,0))] xs
  in show part1

data Move = Move Direction Int

data Direction = Up | Down | Left | Right
  deriving Eq

-- | Rope position
type RP =
  ( (Integer, Integer) -- | Head
  , (Integer, Integer) -- | Tail
  )

parseMove :: String -> Move
parseMove (x:' ':y) = Move (f x) (read y) where
  f = \case
    'R' -> Right
    'U' -> Up
    'L' -> Left
    'D' -> Down
    _ -> error ""
parseMove _ = error ""

trail :: RP -> Move -> [RP]
trail rp (Move d i) = foldl' (\acc d' -> singleMove (head acc) d' : acc) [rp] singleMoves
  where
    singleMoves = if i > 1 then replicate i d else [d]

singleMove :: RP -> Direction -> RP
singleMove (h@(x, y),t@(x', y')) d
  | x == x' + 1 && y == y' && d == Right = (move' h d, h)
  | x == x' - 1 && y == y' && d == Left = (move' h d, h)
  | x == x' && y == y' + 1 && d == Up = (move' h d, h)
  | x == x' && y == y' - 1 && d == Down = (move' h d, h)
  | x == x' - 1 && y == y' + 1 && (d == Up || d == Left) = (move' h d, h)
  | x == x' + 1 && y == y' + 1 && (d == Up || d == Right) = (move' h d, h)
  | x == x' + 1 && y == y' - 1 && (d == Down || d == Right) = (move' h d, h)
  | x == x' - 1 && y == y' - 1 && (d == Down || d == Left) = (move' h d, h)
  | otherwise = (move' h d, t)

move' :: (Num a, Num b) => (a, b) -> Direction -> (a, b)
move' (x,y) d' = case d' of
  Right -> (x+1,y)
  Up -> (x,y+1)
  Left -> (x-1,y)
  Down -> (x,y-1)