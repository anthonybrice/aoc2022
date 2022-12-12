module Day11Spec (spec) where

import Test.Hspec
import Day11 (day11, doTurn, doRound)
import Data.Map (Map, fromList)
import qualified Data.Map as Map
import Day11.Monkey (Monkey(..))
import Util.Parser (parseMaybe)
import Day11.Parse (parseInput)
import Data.Maybe (fromJust)
import Data.List (foldl')

spec :: Spec
spec = do
  exampleInput <- runIO $ readFile "test/input/day11/input"
  let xs = (fromList . zip [0..] . fromJust $ parseMaybe parseInput exampleInput) :: Map Int Monkey
      is = foldl' (\acc i -> Map.insert i (0 :: Int) acc) mempty [0..length xs - 1]
  -- putStrLn example

  -- describe "doTurn" $ do
  --   it "does one monkey's turn" $
  --     doTurn xs is 0 `shouldBe` (mempty, mempty)

  -- describe "doRound" $ do
  --   it "does a full round" $
  --     doRound xs is `shouldBe` (mempty, mempty)

  describe "day11" $ do
    it "solves the example input" $ do
      day11 exampleInput `shouldBe` "10605\n2713310158"

      -- day11 exampleInput `shouldBe` "88\n36"

instance Eq Monkey where
  Monkey i is _ _ == Monkey i' is' _ _ = i == i' && is == is'