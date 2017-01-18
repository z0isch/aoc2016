module Day24.Test where

import           Day24
import           Test.Hspec

tests :: SpecWith ()
tests = do
  describe "Part1" $ do
    it "Test1" $ do
      part1 test1 `shouldBe` Just 14
  describe "Part2" $ do
    it "Test1" $ do
      part2 test1 `shouldBe` Just 20
