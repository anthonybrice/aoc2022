{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day2 (day2) where

day2 :: String -> String
day2 input = 
  let xs = lines input
      part1 = sum $ map (rpsScore . part1Parse) xs
      part2 = sum $ map (rpsScore . part2Parse) xs
  in show part1 <> "\n" <> show part2

data Move = Rock | Paper | Scissors deriving Show

data RpsGame = RpsGame Move Move deriving Show

rpsScore :: RpsGame -> Integer
rpsScore (RpsGame x y) = case (x, y) of
  (Rock, Rock) -> 4
  (Rock, Paper) -> 8
  (Rock, Scissors) -> 3
  (Paper, Rock) -> 1
  (Paper, Paper) -> 5
  (Paper, Scissors) -> 9
  (Scissors, Rock) -> 7
  (Scissors, Paper) -> 2
  (Scissors, Scissors) -> 6

player1Parse :: Char -> Move
player1Parse x = case x of 'A' -> Rock; 'B' -> Paper; 'C' -> Scissors

player2Parse :: Char -> Move
player2Parse x = case x of 'X' -> Rock; 'Y' -> Paper; 'Z' -> Scissors

part1Parse :: String -> RpsGame
part1Parse x = RpsGame (player1Parse $ head x) (player2Parse $ last x)

part2Parse :: String -> RpsGame
part2Parse x = 
  let p1 = player1Parse $ head x
  in RpsGame p1 (endRound p1 $ parseRpsResult $ last x)

data RpsResult = Win | Lose | Draw

parseRpsResult :: Char -> RpsResult
parseRpsResult x = case x of 'X' -> Lose; 'Y' -> Draw; 'Z' -> Win

endRound :: Move -> RpsResult -> Move
endRound Rock x = case x of Lose -> Scissors; Draw -> Rock; Win -> Paper
endRound Paper x = case x of Lose -> Rock; Draw -> Paper; Win -> Scissors
endRound Scissors x = case x of Lose -> Paper; Draw -> Scissors; Win -> Rock
