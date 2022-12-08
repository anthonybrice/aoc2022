module Day7.Types (TerminalOutput(..), Command(..), LsOutput(..)) where

data TerminalOutput =
  Com Command
  | Ls LsOutput
  deriving Show

data Command =
  Cd String
  | LsCommand
  deriving Show

data LsOutput =
  Directory String
  | File Integer String
  deriving Show