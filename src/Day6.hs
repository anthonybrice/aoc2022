{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Day6 (day6) where
import Data.List (nub)

day6 :: String -> String
day6 input = show (findPacketMarker input) <> "\n" <> show (findMessageMarker input)

findPacketMarker :: String -> Int
findPacketMarker xs = f xs 4 where
  f xs@(a:b:c:d:_) n =
    if nub [a,b,c,d] == [a,b,c,d]
      then n
      else f (tail xs) (n+1)

findMessageMarker :: String -> Int
findMessageMarker xs = f xs 14 where
  f xs n =
    if nub (take 14 xs) == take 14 xs
      then n
      else f (tail xs) (n+1)