module Day8Spec (spec) where

import Test.Hspec
import Day8 (day8)

spec :: Spec
spec = do
  describe "day8" $ do
    it "solves the example input" $
      day8 exampleInput `shouldBe` "21\n8"
        where exampleInput = init $ unlines
                [ "30373"
                , "25512"
                , "65332"
                , "33549"
                , "35390"
                ]