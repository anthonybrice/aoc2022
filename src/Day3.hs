{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day3 (day3) where
  
import Data.List (intersect)
import Data.Char (ord, isUpper)

day3 :: String -> String
day3 input = 
  let xs = lines input
  in show (sum $ map (priority . checkRucksack) xs)
    <> "\n"
    <> show (sum . map (priority . findBadge) $ triple xs)

checkRucksack :: String -> Char
checkRucksack xs = 
  let l = length xs `div` 2
      compartment1 = take l xs
      compartment2 = drop l xs
  in head $ intersect compartment1 compartment2

findBadge :: (String, String, String) -> Char
findBadge (a, b, c) = head . intersect a $ intersect b c

priority :: Char -> Integer
priority x 
  | isUpper x = toInteger (ord x) - 38
  | otherwise = toInteger (ord x) - 96

triple :: [a] -> [(a,a,a)]
triple [] = []
triple (a:b:c:ds) = (a,b,c) : triple ds