module Day11.Test where

import           Day11
import           Test.Hspec

tests :: SpecWith ()
tests = do
  describe "Part1" $ do
    it "Test1" $ do
      part1 testStartState test1 `shouldBe` (Just 11)
