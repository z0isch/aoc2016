module Day22.Test where

import           Day22
import           Test.Hspec

tests :: SpecWith ()
tests = do
  describe "Part2" $ do
    it "Test1" $ do
      part2 test1 `shouldBe` Just 7
      part2 test2 `shouldBe` Just 13
      part2 test3 `shouldBe` Just 19
