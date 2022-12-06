module Day5.ParserSpec (spec) where

import Test.Hspec
import Day5.Parser (crate, rowCrate, rowCrates, parseInput, parseMaybe)
import Day5.Types (Move(Move))
import Text.ParserCombinators.ReadP (readP_to_S)
-- import Data.Maybe (Maybe(Nothing))

spec :: Spec
spec = do
  describe "crate" $ do
    it "parses a spot with no crate" $ do
      parseMaybe crate "   " `shouldBe` Just Nothing

    it "parses a crate" $ do
      parseMaybe crate "[A]" `shouldBe` Just (Just 'A')

  describe "rowCrate" $ do
    it "parses a row of crates" $
      last (readP_to_S rowCrate "[A]     [B] [C]         [D]") `shouldBe` (
        [ Just 'A'
        , Nothing
        , Just 'B'
        , Just 'C'
        , Nothing
        , Nothing
        , Just 'D'
        ], "")

  describe "rowCrates" $ do
    it "parses multiple rows of crates" $
      last (readP_to_S rowCrates "[A]     [B] [C]         [D]\n[A]     [B] [C]         [D]") `shouldBe` (
        [ [ Just 'A'
          , Nothing
          , Just 'B'
          , Just 'C'
          , Nothing
          , Nothing
          , Just 'D'
          ]
        , [ Just 'A'
          , Nothing
          , Just 'B'
          , Just 'C'
          , Nothing
          , Nothing
          , Just 'D'
          ]
        ], "")

  describe "parseInput" $
    it "parses the example input" $
      parseMaybe parseInput exampleInput `shouldBe` Just (
        [ [ 'N'
          , 'Z'
          ]
        , [ 'D'
          , 'C'
          , 'M'
          ]
        , [ 'P'
          ]
        ],
          [ Move 1 2 1
          , Move 3 1 3
          , Move 2 2 1
          , Move 1 1 2
          ]
      )
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