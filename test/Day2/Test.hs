module Day2.Test where

import           Day2
import           Test.Hspec

tests :: SpecWith ()
tests = do
  describe "Part1" $ do
    it "Test1" $ do
      part1 test1 `shouldBe` [1,9,8,5]
  describe "Part2" $ do
    it "Test1" $ do
      part2 test1 `shouldBe` [5,13,11,3]
