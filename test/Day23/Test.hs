module Day23.Test where

import qualified Data.Map.Strict as M
import           Day23
import           Test.Hspec

tests :: SpecWith ()
tests = do
  describe "Part1" $ do
    it "Test1" $ do
      part1 M.empty 'a' test1 `shouldBe` 3
  describe "Part2" $ do
    it "Test1" $ do
      part2 M.empty 'a' test1 `shouldBe` 3
