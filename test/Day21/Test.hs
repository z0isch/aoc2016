module Day21.Test where

import           Day21
import           Test.Hspec

tests :: SpecWith ()
tests = do
  describe "Part1" $ do
    it "SwapPos" $ do
      doInstruction "abcde" (SwapPos 4 0) `shouldBe` "ebcda"
      doInstruction "abcde" (SwapPos 1 3) `shouldBe` "adcbe"
    it "SwapLet" $ do
      doInstruction "ebcda" (SwapLet 'd' 'b') `shouldBe` "edcba"
      doInstruction "ebcda" (SwapLet 'e' 'a') `shouldBe` "abcde"
    it "Reverse" $ do
      doInstruction "edcba" (Reverse 0 4) `shouldBe` "abcde"
      doInstruction "abcde" (Reverse 1 3) `shouldBe` "adcbe"
    it "RotateL" $ do
      doInstruction "abcde" (RotateL 1) `shouldBe` "bcdea"
      doInstruction "abcde" (RotateL 5) `shouldBe` "abcde"
      doInstruction "abcde" (RotateL 7) `shouldBe` "cdeab"
    it "RotateR" $ do
      doInstruction "abcde" (RotateR 1) `shouldBe` "eabcd"
      doInstruction "abcde" (RotateR 5) `shouldBe` "abcde"
      doInstruction "abcdefgh" (RotateR 6) `shouldBe` "cdefghab"
      doInstruction "abcde" (RotateR 7) `shouldBe` "deabc"
    it "Move" $ do
      doInstruction "bcdea" (Move 1 4) `shouldBe` "bdeac"
      doInstruction "bdeac" (Move 3 0) `shouldBe` "abdec"
      doInstruction "abcde" (Move 0 3) `shouldBe` "bcdae"
      doInstruction "dacfhgeb" (Move 1 4) `shouldBe` "dcfhageb"
      doInstruction "abcdefgh" (Move 4 1) `shouldBe` "aebcdfgh"
    it "RotateRBase" $ do
      doInstruction "abcde" (RotateRBase 'a') `shouldBe` "eabcd"
      doInstruction "abcde" (RotateRBase 'b') `shouldBe` "deabc"
      doInstruction "abcde" (RotateRBase 'c') `shouldBe` "cdeab"
      doInstruction "abcde" (RotateRBase 'd') `shouldBe` "bcdea"
      doInstruction "abcde" (RotateRBase 'e') `shouldBe` "eabcd"
      doInstruction "ecabd" (RotateRBase 'd') `shouldBe` "decab"
      doInstruction "abcde" (RotateRBase 'a') `shouldBe` "eabcd"
      doInstruction "abcdefgh" (RotateRBase 'e') `shouldBe` "cdefghab"
      doInstruction "abcdefgh" (RotateRBase 'f') `shouldBe` "bcdefgha"
      doInstruction "abcdefgh" (RotateRBase 'g') `shouldBe` "abcdefgh"
      doInstruction "abcdefgh" (RotateRBase 'h') `shouldBe` "habcdefg"
    it "TestInstructions" $ do
      part1 test1 testInstructions1 `shouldBe` ["abcde","ebcda","edcba","abcde","bcdea","bdeac","abdec","ecabd","decab"]
  describe "Part2" $ do
    it "SwapPos" $ do
      reverseDoInstruction (SwapPos 4 0) "ebcda" `shouldBe` "abcde"
      reverseDoInstruction (SwapPos 1 3) "adcbe" `shouldBe` "abcde"
    it "SwapLet" $ do
      reverseDoInstruction (SwapLet 'd' 'b') "edcba" `shouldBe` "ebcda"
      reverseDoInstruction (SwapLet 'e' 'a') "abcde" `shouldBe` "ebcda"
    it "Reverse" $ do
      reverseDoInstruction (Reverse 0 4) "abcde" `shouldBe` "edcba"
      reverseDoInstruction (Reverse 1 3) "adcbe" `shouldBe` "abcde"
    it "RotateL" $ do
      reverseDoInstruction (RotateL 1) "bcdea" `shouldBe` "abcde"
      reverseDoInstruction (RotateL 5) "abcde"`shouldBe` "abcde"
      reverseDoInstruction (RotateL 7) "cdeab" `shouldBe` "abcde"
    it "RotateR" $ do
      reverseDoInstruction (RotateR 1) "eabcd" `shouldBe` "abcde"
      reverseDoInstruction (RotateR 5) "abcde" `shouldBe` "abcde"
      reverseDoInstruction (RotateR 6) "cdefghab" `shouldBe` "abcdefgh"
      reverseDoInstruction (RotateR 7) "deabc" `shouldBe` "abcde"
    it "Move" $ do
      reverseDoInstruction (Move 1 4) "bdeac" `shouldBe` "bcdea"
      reverseDoInstruction (Move 3 0) "abdec" `shouldBe` "bdeac"
      reverseDoInstruction (Move 0 3) "bcdae" `shouldBe` "abcde"
      reverseDoInstruction (Move 1 4) "dcfhageb" `shouldBe` "dacfhgeb"
      reverseDoInstruction (Move 4 1) "aebcdfgh" `shouldBe` "abcdefgh"
    it "RotateRBase" $ do
      reverseDoInstruction (RotateRBase 'a') "habcdefg" `shouldBe` "abcdefgh"
      reverseDoInstruction (RotateRBase 'b') "ghabcdef" `shouldBe` "abcdefgh"
      reverseDoInstruction (RotateRBase 'c') "fghabcde" `shouldBe` "abcdefgh"
      reverseDoInstruction (RotateRBase 'd') "efghabcd" `shouldBe` "abcdefgh"
      reverseDoInstruction (RotateRBase 'e') "cdefghab" `shouldBe` "abcdefgh"
      reverseDoInstruction (RotateRBase 'f') "bcdefgha" `shouldBe` "abcdefgh"
      reverseDoInstruction (RotateRBase 'g') "abcdefgh" `shouldBe` "abcdefgh"
      reverseDoInstruction (RotateRBase 'h') "habcdefg" `shouldBe` "abcdefgh"
