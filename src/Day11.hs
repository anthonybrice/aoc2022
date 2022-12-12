{-# LANGUAGE NumericUnderscores #-}
module Day11 (day11, doTurn, doRound) where

import Day11.Parse (parseInput)
import Util.Parser (parseMaybe)
import Data.Maybe (fromJust)
import Day11.Monkey (Monkey(..), Test (..))
import Data.Map (Map, fromList, toList, size, elems)
import qualified Data.Map as Map
import Data.List (foldl', sortBy)

day11 :: String -> String
day11 input =
  let xs = (fromList . zip [0..] . fromJust $ parseMaybe parseInput input) :: Map Int Monkey
      is = foldl' (\acc i -> Map.insert i 0 acc) mempty [0..length xs - 1]
      part1 = product . take 2 . sortBy (flip compare) . map snd . toList . fst $ foldl' (\(is', ms) _ -> doRound ms is') (is, xs) $ replicate 20 ()
      part2 = product . take 2 . sortBy (flip compare) . map snd . toList . fst $ foldl' (\(is', ms) _ -> doRound' ms is') (is, xs) $ replicate 10_000 ()

  in show part1 <> "\n" <> show part2

doRound :: Map Int Monkey -> Map Int Integer -> (Map Int Integer, Map Int Monkey)
doRound ms is =
  let ml = [0..size ms - 1]

  in foldl' (\(is', ms') m -> doTurn ms' is' m) (is, ms) ml

doRound' :: Map Int Monkey -> Map Int Integer -> (Map Int Integer, Map Int Monkey)
doRound' ms is =
  let ml = [0..size ms - 1]
      lcmm = foldr lcm 1
      n' = lcmm $ map (\(Monkey _ _ _ (Test _ _ _ n)) -> n) $ elems ms

  in foldl' (\(is', ms') m -> doTurn' ms' is' m n') (is, ms) ml

doTurn :: Map Int Monkey -> Map Int Integer -> Int -> (Map Int Integer, Map Int Monkey)
doTurn ms m index =
  let (Monkey i is op (Test t m1 m2 n)) = fromJust $ Map.lookup index ms
      m' = Map.insert i (fromIntegral (length is) + fromJust (Map.lookup i m)) m

      xs = map ((`div` 3) . op) is

      doThrows = foldl' (\acc item ->
        let true = fromJust $ Map.lookup m1 acc
            false = fromJust $ Map.lookup m2 acc
        in if t item
            then Map.insert m1 (throw item true) acc
            else Map.insert m2 (throw item false) acc) ms xs

  in (m', Map.insert i (Monkey i [] op (Test t m1 m2 n)) doThrows)

doTurn' :: Map Int Monkey -> Map Int Integer -> Int -> Integer -> (Map Int Integer, Map Int Monkey)
doTurn' ms m index lcm' =
  let (Monkey i is op (Test t m1 m2 n)) = fromJust $ Map.lookup index ms
      m' = Map.insert i (fromIntegral (length is) + fromJust (Map.lookup i m)) m

      xs = map ((`mod` lcm') . op) is

      doThrows = foldl' (\acc item ->
        let true = fromJust $ Map.lookup m1 acc
            false = fromJust $ Map.lookup m2 acc
        in if t item
            then Map.insert m1 (throw item true) acc
            else Map.insert m2 (throw item false) acc) ms xs

  in (m', Map.insert i (Monkey i [] op (Test t m1 m2 n)) doThrows)

throw :: Integer -> Monkey -> Monkey
throw item (Monkey i is op t) = Monkey i (is <> [item]) op t