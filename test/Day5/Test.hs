module Day5.Test where

import           Day5
import           Test.Hspec

tests :: SpecWith ()
tests = do
  describe "Part1" $ do
    it "Test1" $ do
      part1 test1 `shouldBe` "18f47a30"
  describe "Part2" $ do
    it "Test1" $ do
      part2 test1 `shouldBe` "05ace8e3"
