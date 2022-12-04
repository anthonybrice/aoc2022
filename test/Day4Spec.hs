module Day4Spec (spec) where

import Test.Hspec
import Day4 (day4)

spec :: Spec
spec = 
    describe "day4" $ do
      it "solves the example input" $
        day4 exampleInput `shouldBe` "2\n4"

exampleInput :: String
exampleInput = "2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8"