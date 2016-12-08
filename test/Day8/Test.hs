module Day8.Test where

import           Day8
import           Test.Hspec

tests :: SpecWith ()
tests = do
  describe "Part1" $ do
    it "Test1" $ do
      part1 smallScreen test1 `shouldBe` 6
