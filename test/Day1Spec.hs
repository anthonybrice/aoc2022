module Day1Spec (spec) where

import Test.Hspec
import Day1

spec :: Spec
spec = do
  describe "day1" $ do
    it "solves the example input" $ 
      day1 "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000" `shouldBe` "24000\n45000"