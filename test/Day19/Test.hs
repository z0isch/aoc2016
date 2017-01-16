module Day19.Test where

import           Day19
import           Test.Hspec

tests :: SpecWith ()
tests = do
  describe "Part1" $ do
    it "Test1" $ do
      part1 test1 `shouldBe` 3
      part1 4 `shouldBe` 1
      part1 6 `shouldBe` 5
      part1 20 `shouldBe` 9
      part1' 20 `shouldBe` 9
  describe "Part2" $ do
    it "Test1" $ do
      part2 test1 `shouldBe` 2
      part2 4 `shouldBe` 1
      part2 6 `shouldBe` 3
      part2 7 `shouldBe` 5
      part2 10 `shouldBe` 1
      part2' 10 `shouldBe` 1
