module Day6.Test where

  import           Day6
  import           Test.Hspec

  tests :: SpecWith ()
  tests = do
    describe "Part1" $ do
      it "Test1" $ do
        part1 (lines test1) `shouldBe` "easter"
    describe "Part2" $ do
      it "Test1" $ do
        part2 (lines test1) `shouldBe` "advent"
