module Day17.Test where

import           Day17
import           Test.Hspec

tests :: SpecWith ()
tests = do
  describe "Part1" $ do
    it "Test1" $ do
      part1 test1 `shouldBe` "DDRRRD"
    it "Test2" $ do
      part1 test2 `shouldBe` "DDUDRLRRUDRD"
    it "Test3" $ do
      part1 test3 `shouldBe` "DRURDRUDDLLDLUURRDULRLDUUDDDRR"
    it "Input" $ do
      part1 input1 `shouldBe` "DUDDRLRRRD"
  describe "Part2" $ do
    it "Test1" $ do
      part2 test1 `shouldBe` 370
    it "Test2" $ do
      part2 test2 `shouldBe` 492
    it "Test3" $ do
      part2 test3 `shouldBe` 830
    it "Input" $ do
      part2 input1 `shouldBe` undefined
