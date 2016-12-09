module Day9.Test where

import           Day9
import           Test.Hspec

tests :: SpecWith ()
tests = do
  describe "Part1" $ do
    it "Test1" $ do
      part1 test1 `shouldBe` "ADVENTABBBBBCXYZXYZXYZABCBCDEFEFG(1x3)AX(3x3)ABC(3x3)ABCY"
  describe "Part2" $ do
    it "Test1" $ do
      part2 test2 `shouldBe` (9+20+241920+445)
