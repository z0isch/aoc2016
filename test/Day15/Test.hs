module Day15.Test where

import           Day15
import           Test.Hspec

tests :: SpecWith ()
tests = do
  describe "Part1" $ do
    it "Test1" $ do
      part1 test1 `shouldBe` 5
