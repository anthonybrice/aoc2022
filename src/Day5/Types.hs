module Day5.Types (Crate, Stack, Move(..)) where

type Crate = Char

type Stack = [Crate]

data Move = Move
  { count :: Int
  , from :: Int
  , to :: Int
  }
  deriving (Show, Eq)