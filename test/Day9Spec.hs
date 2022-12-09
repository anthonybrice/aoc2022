module Day9Spec (spec) where

import Prelude hiding (Right, Left)
import Test.Hspec
import Day9 (day9, Direction(..), singleMove)

spec :: Spec
spec = do
  describe "day9" $ do
    it "solves the example input" $
      day9 exampleInput `shouldBe` "13"


  describe "singleMove" $ do
    it "works when the head covers the tail" $
      map (singleMove ((0,0),(0,0))) ds `shouldBe` [((0,1),(0,0)), ((0,-1),(0,0)), ((-1,0),(0,0)), ((1,0),(0,0))]

    it "works when the head is to just right of the tail" $
      map (singleMove ((1,0),(0,0))) ds `shouldBe` [((1,1),(0,0)), ((1,-1),(0,0)), ((0,0),(0,0)), ((2,0),(1,0))]

    it "works when the head is just left of the tail" $
      map (singleMove ((-1,0),(0,0))) ds `shouldBe` [((-1,1),(0,0)), ((-1,-1),(0,0)), ((-2,0),(-1,0)), ((0,0),(0,0))]

    it "works when the head is just above the tail" $
      map (singleMove ((0,1),(0,0))) ds `shouldBe` [((0,2),(0,1)), ((0,0),(0,0)), ((-1,1),(0,0)), ((1,1),(0,0))]

    it "works when the head is just below the tail" $
      map (singleMove ((0,-1),(0,0))) ds `shouldBe` [((0,0),(0,0)), ((0,-2),(0,-1)), ((-1,-1),(0,0)), ((1,-1),(0,0))]

    it "works when the head is above left of the tail" $
      map (singleMove ((-1,1),(0,0))) ds `shouldBe` [((-1,2),(-1,1)), ((-1,0),(0,0)), ((-2,1),(-1,1)), ((0,1),(0,0))]

    it "works when the head is above right of the tail" $
      map (singleMove ((1,1),(0,0))) ds `shouldBe` [((1,2),(1,1)), ((1,0),(0,0)), ((0,1),(0,0)), ((2,1),(1,1))]

    it "works when the head is below right of the tail" $
      map (singleMove ((1,-1),(0,0))) ds `shouldBe` [((1,0),(0,0)), ((1,-2),(1,-1)), ((0,-1),(0,0)), ((2,-1),(1,-1))]

    it "works when the head is below left of the tail" $
      map (singleMove ((-1,-1),(0,0))) ds `shouldBe` [((-1,0),(0,0)), ((-1,-2),(-1,-1)), ((-2,-1),(-1,-1)), ((0,-1),(0,0))]

ds = [Up, Down, Left, Right]

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

