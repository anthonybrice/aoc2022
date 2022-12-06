module Day5Spec (spec) where

import Test.Hspec
import Day5 (day5)

spec :: Spec
spec =
    describe "day5" $ do
      it "solves the example input" $
        day5 exampleInput `shouldBe` "CMZ\nMCD"
        where
          exampleInput = init $ unlines
            [ "    [D]    "
            , "[N] [C]    "
            , "[Z] [M] [P]"
            , " 1   2   3 "
            , ""
            , "move 1 from 2 to 1"
            , "move 3 from 1 to 3"
            , "move 2 from 2 to 1"
            , "move 1 from 1 to 2"
            ]