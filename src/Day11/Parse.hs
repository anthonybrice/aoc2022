{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}

module Day11.Parse (parseInput) where

import Day11.Monkey (Monkey(..), Test(..))
import Text.ParserCombinators.ReadP (ReadP, string, char, sepBy, get, look, eof)
import Util.Parser (integer)

parseInput :: ReadP [Monkey]
parseInput = do
  ms <- sepBy parseMonkey (string "\n\n")
  eof
  return ms

parseMonkey :: ReadP Monkey
parseMonkey = do
  _ <- string "Monkey "
  i <- fromInteger <$> integer
  _ <- string ":\n"
  _ <- string "  Starting items: "
  is <- parseItems
  _ <- char '\n'
  op <- parseOperation
  _ <- char '\n'
  test <- parseTest

  return $ Monkey i is op test

parseItems :: ReadP [Integer]
parseItems = sepBy (fromInteger <$> integer) (string ", ")

parseOperation :: ReadP (Integer -> Integer)
parseOperation = do
  _ <- string "  Operation: new = old "
  c <- get
  let op = case c of '+' -> (+); '-' -> (-); '*' -> (*); _ -> error "bad parse"
  _ <- char ' '
  s <- take 4 <$> look
  if s == "old\n"
    then do
      _ <- string "old"
      return $ \x -> x `op` x
    else do
      y <- fromInteger <$> integer
      return (`op` y)

parseTest :: ReadP Test
parseTest = do
  _ <- string "  Test: divisible by "
  n <- integer
  _ <- char '\n'
  _ <- string "    If true: throw to monkey "
  m1 <- fromInteger <$> integer
  _ <- char '\n'
  _ <- string "    If false: throw to monkey "
  m2 <- fromInteger <$> integer

  return $ Test (\x -> x `rem` n == 0) m1 m2 n


