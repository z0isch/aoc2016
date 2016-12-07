module Day7.Test where

import           Day7
import           Test.Hspec

tests :: SpecWith ()
tests = do
  describe "Part1" $ do
    it "Test1" $ do
      part1 test1 `shouldBe` [True, False, False, True]
  describe "Part2" $ do
    it "Test1" $ do
      part2 test2 `shouldBe` [True, False, True, True]
