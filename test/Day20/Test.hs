module Day20.Test where

import           Day20
import           Test.Hspec
import           Test.QuickCheck

tests :: SpecWith ()
tests = do
  describe "Part1" $ do
    it "Test1" $ do
      part1 test1 `shouldBe` 3
  describe "Part2" $ do
    it "Restricted" $ do
      numRestricted [(0,2),(5,8),(4,7)] `shouldBe` 8
      numRestricted [(5,8),(0,2),(4,7)] `shouldBe` 8
      numRestricted [(4,7),(0,2),(5,8)] `shouldBe` 8
      numRestricted [(0,2),(1,4),(3,9)] `shouldBe` 10
      numRestricted [(0,2),(1,4),(3,9),(0,9)] `shouldBe` 10
      numRestricted [(4,7),(0,2),(5,8),(9,10)] `shouldBe` 10
      numRestricted [(9,10),(4,7),(0,2),(5,8)] `shouldBe` 10
      numRestricted [(1,2),(1,2)] `shouldBe` 2
      numRestricted [(3,5),(4,5),(3,4),(4,5)] `shouldBe` 3
    it "Num Restricted <= Range" $
      property $
      numRestrictedProp (0,10000)
    it "Tes1" $ do
      part2 range1 test1 `shouldBe` 2

numRestrictedProp :: (Integer,Integer) -> Property
numRestrictedProp (mi,mx) = forAll (listOf $ tupleInRange (mi,mx)) $
  \xs -> (mx - mi + 1) >= numRestricted xs

tupleInRange :: (Integer,Integer) -> Gen (Integer,Integer)
tupleInRange (x,y) = do
  mi <- elements [x..y-1]
  mx <- elements [mi+1..y]
  return (mi,mx)
