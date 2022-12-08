module Util.Parser (parseMaybe, word, integer, restOfLine) where

import Text.ParserCombinators.ReadP (ReadP, readP_to_S, munch, look, get)
import Data.Char (isAlpha, isDigit)

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
  case readP_to_S parser input of
    (result, []) : _ -> Just result
    _ -> Nothing

word :: ReadP String
word = munch isAlpha

integer :: ReadP Integer
integer = read <$> munch isDigit

restOfLine :: ReadP String
restOfLine = do
  s <- look
  f s [] where
    f (c:s) acc | c /= '\n' = do _ <- get; f s (acc <> [c])
    f _ acc = return acc