module Day16.Test where

import           Day16
import           Test.Hspec

tests :: SpecWith ()
tests = do
  describe "Part1" $ do
    it "Test1" $ do
      part1 20 test1 `shouldBe` "01100"
