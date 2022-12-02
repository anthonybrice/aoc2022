module Day2Spec (spec) where

import Test.Hspec

import Day2 (day2)

spec :: Spec
spec = do
  describe "day2" $ do
    it "solves the example input" $ 
      day2 "A Y\nB X\nC Z" `shouldBe` "15\n12"