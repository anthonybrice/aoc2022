module Day11.Monkey (Monkey(..), Test(..)) where

data Monkey = Monkey
  Int -- | index
  [Integer] -- items
  (Integer -> Integer) -- operation
  Test

data Test = Test
  (Integer -> Bool) -- test function
  Int -- if true, throw to
  Int -- if false, throw to
  Integer -- divisor

instance Show Monkey where
  show (Monkey i is _ t) = "Monkey " <> show i <> " " <> show is <> " _ (" <> show t <> ")"

instance Show Test where
  show (Test _ m1 m2 n) = "Test _ " <> show m1 <> " " <> show m2 <> " " <> show n