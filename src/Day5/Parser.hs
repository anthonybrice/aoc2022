{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}

module Day5.Parser (crate, rowCrate, rowCrates, parseInput, parseMaybe) where

import Text.ParserCombinators.ReadP (ReadP, readP_to_S, get, satisfy, sepBy1, look, string, munch1, pfail, eof)
import Day5.Types (Crate, Move(Move), Stack)
import Data.Char (isDigit, isAlpha)
import Data.List (transpose)
import Data.Maybe (catMaybes)

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
  case readP_to_S parser input of
    (result, []) : _ -> Just result
    _ -> Nothing

type RowCrate = [Maybe Crate]

rowCrate :: ReadP RowCrate
rowCrate = sepBy1 crate space

rowCrates :: ReadP [RowCrate]
rowCrates = sepBy1 rowCrate $ satisfy (== '\n')

parseInput :: ReadP ([Stack], [Move])
parseInput = do
  rows <- rowCrates
  _ <- satisfy (== '\n')
  skipLine
  _ <- satisfy (== '\n')
  ys <- moves
  eof
  return (map catMaybes $ transpose rows, ys)

moves :: ReadP [Move]
moves = sepBy1 move $ satisfy (== '\n')

move :: ReadP Move
move = do
  _ <- string "move "
  count <- integer
  _ <- string " from "
  from <- integer
  _ <- string " to "
  to <- integer
  return $ Move count from to

skipLine :: ReadP ()
skipLine = do
  s <- look
  skip s where
    skip (c:s) | c /= '\n' = do _ <- get; skip s
    skip _ = do _ <- get; return ()

crate :: ReadP (Maybe Crate)
crate = do
  _ <- get
  c <- get
  _ <- get
  case c of
    _ | c == ' ' -> return Nothing
    _ | isAlpha c -> return $ Just c
    _ -> pfail

space :: ReadP Char
space = satisfy (== ' ')

integer :: ReadP Int
integer = read <$> munch1 isDigit

