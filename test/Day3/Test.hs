module Day3.Test where

import qualified Data.Set   as Set
import           Day3
import           Test.Hspec

tests :: SpecWith ()
tests = do
  describe "Part1" $ do
    it "Test1" $ do
      part1 test1 `shouldBe` 0
  describe "Part2" $ do
    it "Test1" $ do
      let t = makeSet $ concatMap makeTriples $ part2Parser test2
          e = makeSet [(101,102,103),(201,202,203),(301,302,303),(401,402,403),(501,502,503),(601,602,603)]
      t `shouldBe` e


untupulize :: (a,a,a) -> [a]
untupulize (a,b,c) = [a,b,c]

makeSet :: (Ord a) => [(a,a,a)] -> Set.Set (Set.Set a)
makeSet = Set.fromList . map (Set.fromList . untupulize)
