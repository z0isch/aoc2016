module Day18.Test where

import           Day18
import           Test.Hspec

tests :: SpecWith ()
tests = do
  describe "Part1" $ do
    it "GenNext" $ do
      genNext test1 `shouldBe` "^^^...^..^"
    it "Test1" $ do
      part1 test1 10 `shouldBe` 38
    it "Input" $ do
      part1 input1 40 `shouldBe` 1963
  describe "Part2" $ do
    it "Input" $ do
      part2 input1 400000 `shouldBe` 20009568
