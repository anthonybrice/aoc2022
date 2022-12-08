module Day7Spec (spec) where

import Test.Hspec
import Day7 (day7)

spec :: Spec
spec =
    describe "day7" $ do
      it "solves the example input" $
        day7 exampleInput `shouldBe` "95437\n24933642"

exampleInput = init $ unlines
  [ "$ cd /"
  , "$ ls"
  , "dir a"
  , "14848514 b.txt"
  , "8504156 c.dat"
  , "dir d"
  , "$ cd a"
  , "$ ls"
  , "dir e"
  , "29116 f"
  , "2557 g"
  , "62596 h.lst"
  , "$ cd e"
  , "$ ls"
  , "584 i"
  , "$ cd .."
  , "$ cd .."
  , "$ cd d"
  , "$ ls"
  , "4060174 j"
  , "8033020 d.log"
  , "5626152 d.ext"
  , "7214296 k"
  ]
