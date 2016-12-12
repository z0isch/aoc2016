module Day10.Test where

import           Day10
import           Test.Hspec

tests :: SpecWith ()
tests = do
  describe "Part1" $ do
    it "Test1" $ do
      part1  5 2 test1 `shouldBe` (Bot 2)
