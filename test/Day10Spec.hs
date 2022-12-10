module Day10Spec (spec) where

import Test.Hspec
import Day10 (day10)

spec :: Spec
spec = do
  describe "day10" $ do
    it "solves the example input" $
      day10 "" `shouldBe` "13140"