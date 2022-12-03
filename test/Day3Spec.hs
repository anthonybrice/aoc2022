module Day3Spec (spec) where

import Test.Hspec

import Day3 (day3)

spec :: Spec
spec = 
  describe "day3" $ do
    it "solves the example input" $
      day3 exampleInput `shouldBe` "157\n70"

exampleInput :: String
exampleInput = "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw"