module Day7.Parser (parseInput) where

import Text.ParserCombinators.ReadP (ReadP, sepBy, (+++), satisfy, string, char, eof)
import Day7.Types (TerminalOutput(..), Command(..), LsOutput(..))
import Util.Parser (integer, restOfLine)

parseInput :: ReadP [TerminalOutput]
parseInput = do
  out <- sepBy (command +++ lsOutput) $ satisfy (== '\n')
  eof
  return out

command :: ReadP TerminalOutput
command = do
  _ <- string "$ "
  ls +++ cd

ls :: ReadP TerminalOutput
ls = do
  _ <- string "ls"
  return $ Com LsCommand

cd :: ReadP TerminalOutput
cd = do
  _ <- string "cd "
  Com . Cd <$> restOfLine

lsOutput :: ReadP TerminalOutput
lsOutput = Ls <$> file +++ dir

file :: ReadP LsOutput
file = do
  size <- integer
  _ <- char ' '
  File size <$> restOfLine

dir :: ReadP LsOutput
dir = do
  _ <- string "dir "
  Directory <$> restOfLine