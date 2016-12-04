module Day4.Test where

import           Day4
import           Test.Hspec

tests :: SpecWith ()
tests = do
  describe "Part1" $ do
    it "Test1" $ do
      part1 test1 `shouldBe` 1514
  describe "Part2" $ do
    it "Test1" $ do
      breakCode (RoomCode ["qzmt","zixmtkozy","ivhz"] 343 "") `shouldBe` "very encrypted name"
