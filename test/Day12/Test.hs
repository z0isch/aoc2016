module Day12.Test where

import           Day12
import           Test.Hspec

tests :: SpecWith ()
tests = do
  describe "Part1" $ do
    it "Test1" $ do
      part1 'a' test1 `shouldBe` 42
