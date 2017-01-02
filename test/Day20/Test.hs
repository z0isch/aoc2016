module Day20.Test where

import           Day20
import           Test.Hspec

tests :: SpecWith ()
tests = do
  describe "Part1" $ do
    it "Test1" $ do
      part1 test1 `shouldBe` 3
  describe "Part2" $ do
    it "Test2" $ do
      part2 test1 `shouldBe` 8
