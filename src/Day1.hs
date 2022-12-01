module Day1 (day1) where

import Data.List.Split (splitWhen)
import Data.List (sortBy)

day1 :: IO ()
day1 = do
  content <- readFile "input/day1a"
  let xs' = lines content
      xs = map (sum . map read) $ splitWhen (== "") xs'
      day1a = maximum xs :: Integer
  print day1a
  let day1b = sum . take 3 $ sortBy (flip compare) xs
  print day1b