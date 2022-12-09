module Day9Spec (spec) where

import Prelude hiding (Right, Left)
import Test.Hspec
import Day9 (day9)

spec :: Spec
spec = do
  describe "day9" $ do
    it "solves the example input" $ do
      day9 exampleInput `shouldBe` "13\n1"

      day9 exampleInput' `shouldBe` "88\n36"

exampleInput :: String
exampleInput = init $ unlines
  [ "R 4"
  , "U 4"
  , "L 3"
  , "D 1"
  , "R 4"
  , "D 1"
  , "L 5"
  , "R 2"
  ]

exampleInput' :: String
exampleInput' = init $ unlines
  [ "R 5"
  , "U 8"
  , "L 8"
  , "D 3"
  , "R 17"
  , "D 10"
  , "L 25"
  , "U 20"
  ]

