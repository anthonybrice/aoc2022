module Day2 (day2, part2Parse) where

day2 :: String -> String
day2 input = 
  let xs' = lines input
      xs = map part1Parse xs'
      ys = map part2Parse xs'
  in show (part1 xs) <> "\n" <> show (part2 ys)

part1 :: [Integer] -> Integer
part1 = sum

part2 :: [Integer] -> Integer
part2 = sum

part1Parse :: String -> Integer
part1Parse x = case x of
  "A X" -> 
    -- ShapeYouSelected 1
    1
    -- OutcomeOfRound
    + 3 -- 4
  "A Y" -> 
    -- ShapeYouSelected
    2
    -- OutcomeOfRound
    + 6 -- 8 
  "A Z" ->
    -- ShapeYouSelected
    3
    -- OutcomeOfRound
    + 0 -- 3
  "B X" -> 
    -- ShapeYouSelected
    1
    -- OutcomeOfRound
    + 0 -- 1
  "B Y" -> 
    -- ShapeYouSelected
    2
    -- OutcomeOfRound
    + 3 -- 5
  "B Z" -> 
    -- ShapeYouSelected
    3
    -- OutcomeOfRound
    + 6 -- 9
  "C X" -> 
    -- ShapeYouSelected
    1
    -- OutcomeOfRound
    + 6 -- 7
  "C Y" -> 
    -- ShapeYouSelected
    2
    -- OutcomeOfRound
    + 0 -- 2
  "C Z" -> 
    --ShapeYouSelected
    3
    -- OutcomeOfRound
    + 3 -- 6
  _ -> 0

part2Parse :: String -> Integer
part2Parse x = case x of
  "A X" -> -- Lose 0 + 3
    3
  "A Y" -> -- Draw 3 + 1
    4
  "A Z" -> -- Win 2 + 6
    8
  "B X" -> -- Lose 0 + 1
    1
  "B Y" ->  -- Draw 2 + 2
    4
  "B Z" -> -- Win 3 + 6
    9 
  "C X" -> -- Lose 3 + 2
    5
  "C Y" -> -- Draw 3 + 3
    6
  "C Z" -> 7
  _ -> -1