{-# LANGUAGE
  LambdaCase,
  NumericUnderscores
  #-}

module Day7 (day7) where

import Control.Monad.State
    ( evalState, MonadState(put, get), State )
import Util.Parser (parseMaybe)
import Day7.Parser (parseInput)
import Day7.Types (TerminalOutput (..), Command (..))
import qualified Day7.Types as Parse
import Data.Maybe (fromJust)
import Data.List (find, deleteBy)

day7 :: String -> String
day7 input =
  let termOut = fromJust $ parseMaybe parseInput input
      root = buildFs termOut
      part1 = sum . filter (<= 100_000) $ totalSizeOfEachDir root
      part2 = sizeToDelete root
  in show part1 <> "\n" <> show part2

totalSize :: FS -> Integer
totalSize (Dir ds fs _) = sum (map (\(File i _) -> i) fs <> map totalSize ds)

totalSizeOfEachDir :: FS -> [Integer]
totalSizeOfEachDir x@(Dir ds _ _) = totalSize x : concatMap totalSizeOfEachDir ds

sizeToDelete :: FS -> Integer
sizeToDelete root =
  let x = totalSize root
      xs = totalSizeOfEachDir root
  in minimum [y | y <- xs, 70_000_000 - x + y >= 30_000_000]

data File = File Integer String
  deriving Show

data Dir = Dir [Dir] [File] String
  deriving Show

type FS = Dir

type TerminalState a = State ([String], FS) a

buildFs :: [TerminalOutput] -> FS
buildFs xs = evalState (filesystem xs) ([], root)
  where root = Dir [] [] "/"

filesystem :: [TerminalOutput] -> TerminalState FS
filesystem [] = do
  (_, root) <- get
  return root

filesystem to = do
  case head to of
    Com (Cd name) -> cd name
    Com LsCommand -> pure ()
    Ls (Parse.Directory name) -> mkdir name
    Ls (Parse.File size name) -> mkfile size name
  filesystem $ tail to

cd :: String -> TerminalState ()
cd "/" = do
  (_, root) <- get
  put ([],  root)
cd ".." = do
  (cwd, root) <- get
  put (tail cwd, root)
cd name = do
  (cwd, root) <- get
  let Dir xs _ _ = if null cwd then root else getDir (reverse cwd) root
      Dir _ _ s = findDir name xs
  put (s : cwd, root)

mkdir :: String -> TerminalState ()
mkdir s = do
  (cwd, root) <- get
  put (cwd, updateDir root (reverse cwd) (Dir [] [] s))

updateDir :: FS -> [String] -> Dir -> FS
updateDir (Dir ds fs n) [] newDir = Dir (newDir : ds) fs n
updateDir (Dir ds fs n) path newDir =
  let nextDir = findDir (head path) ds
  in Dir (updateDir nextDir (tail path) newDir : deleteDir nextDir ds) fs n

findDir :: String -> [Dir] -> Dir
findDir name xs = fromJust $ find (\case Dir _ _ n | n == name -> True; _ -> False) xs

deleteDir :: Dir -> [Dir] -> [Dir]
deleteDir = deleteBy (\(Dir _ _ a) (Dir _ _ b) -> a == b)

getDir :: [String] -> Dir -> Dir
getDir [] dir = dir
getDir (p:ps) (Dir xs _ _) = getDir ps (findDir p xs)

mkfile :: Integer -> String -> TerminalState ()
mkfile i s = do
  (cwd, root) <- get
  put (cwd, updateFile root (reverse cwd) (File i s))

updateFile :: FS -> [String] -> File -> FS
updateFile (Dir ds fs n) [] newFile = Dir ds (newFile : fs) n
updateFile (Dir ds fs n) path newFile =
  let nextDir = findDir (head path) ds
  in Dir (updateFile nextDir (tail path) newFile : deleteDir nextDir ds) fs n
