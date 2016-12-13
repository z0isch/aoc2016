module Day13.Test where

import           Day13
import           Test.Hspec

tests :: SpecWith ()
tests = do
  describe "Part1" $ do
    it "TestMap" $ do
      showMap m `shouldBe` testMap
    it "Test1" $ do
      part1 test1 (1,1) (7,4) `shouldBe` (Just 11)

m =  map (\y -> [getTile test1 (x,y) | x <-[0..9]]) [0..6]
