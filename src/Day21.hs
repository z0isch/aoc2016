module Day21 (part1,part2,test1,test2,testInstructions1, doInstruction, reverseDoInstruction, Instruction(..)) where

import           Data.List
import           Data.Maybe
import Text.Trifecta
import Control.Applicative

data Instruction = SwapPos Int Int
                  | SwapLet Char Char
                  | RotateR Int
                  | RotateL Int
                  | RotateRBase Char
                  | Reverse Int Int
                  | Move Int Int
  deriving (Eq, Show)

int :: Parser Int
int = fromInteger <$> integer

parseInput :: String -> Result [Instruction]
parseInput = parseString (some (instructionParser <* skipOptional windowsNewLine)) mempty
  where windowsNewLine = const () <$ skipOptional newline <*> skipOptional (char '\r')

instructionParser :: Parser Instruction
instructionParser = try moveParser
                    <|> try reverseParser
                    <|> try rotateRBaseParser
                    <|> try rotateRParser
                    <|> try rotateLParser
                    <|> try swapLetParser
                    <|> swapPosParser

moveParser :: Parser Instruction
moveParser = string "move position " *> (Move <$> int <*> secondPart)
  where secondPart = string "to position " *> int

reverseParser :: Parser Instruction
reverseParser = string "reverse positions " *> (Reverse <$> int <*> secondPart)
  where secondPart = string "through " *> int

rotateRBaseParser :: Parser Instruction
rotateRBaseParser =  string "rotate based on position of letter " *> (RotateRBase <$> anyChar)

rotateRParser :: Parser Instruction
rotateRParser =  string "rotate right " *> (RotateR <$> int <* (string "step" <* skipOptional (char 's')))

rotateLParser :: Parser Instruction
rotateLParser =  string "rotate left " *> (RotateL <$> int <* (string "step" <* skipOptional (char 's')))

swapLetParser :: Parser Instruction
swapLetParser =  string "swap letter " *> (SwapLet <$> anyChar <* whiteSpace <*> secondPart)
  where
    secondPart :: Parser Char
    secondPart = string "with letter " *> anyChar

swapPosParser :: Parser Instruction
swapPosParser =  string "swap position " *> (SwapPos <$> int <*> secondPart)
  where
    secondPart = string "with position " *> int

doInstruction :: String -> Instruction -> String
doInstruction s (SwapPos x y) = take (min x y) s
                                ++ [larger]
                                ++ take (max x y - min x y - 1) (drop (min x y+1) s)
                                ++ [smaller]
                                ++ drop (max x y + 1) s
  where
    smaller = s !! min x y
    larger = s !! max x y
doInstruction s (SwapLet x y) = doInstruction s (SwapPos xi yi)
  where
    xi = fromJust $ elemIndex x s
    yi = fromJust $ elemIndex y s
doInstruction s (Reverse x y) = take x s
                              ++ reverse (take (y-x + 1) (drop x s))
                              ++ drop (y+1) s
doInstruction s (RotateL x) = drop (x `mod` length s) s ++ take (x `mod` length s) s
doInstruction s (RotateR x) = reverse $ doInstruction (reverse s) (RotateL x)
doInstruction s (Move x y)
  | x > y = take y removed ++ [s !! x] ++ drop y removed
  | otherwise = take y removed ++ [s !! x] ++ drop y removed
  where removed = take x s ++ drop (x+1) s
doInstruction s (RotateRBase c) = doInstruction s (RotateR rotateTimes)
  where
    ci = fromJust $ elemIndex c s
    rotateTimes
      | ci >= 4 = 2 + ci
      | otherwise = 1 + ci

fromSuccess :: Result x -> x
fromSuccess (Success x) = x
fromSuccess (Failure x) = error (show x)

reverseDoInstruction :: Instruction -> String -> String
reverseDoInstruction (SwapPos x y) s = doInstruction s (SwapPos y x)
reverseDoInstruction (SwapLet x y) s = doInstruction s (SwapLet y x)
reverseDoInstruction (Reverse x y) s = doInstruction s (Reverse x y)
reverseDoInstruction (RotateL x) s = doInstruction s (RotateR x)
reverseDoInstruction (RotateR x) s = doInstruction s (RotateL x)
reverseDoInstruction (Move x y) s = doInstruction s (Move y x)
--Non-deterministic for length 5!
reverseDoInstruction (RotateRBase c) s = fromJust
                                         $ find (\a -> doInstruction a (RotateRBase c) == s)
                                         $ map (doInstruction s . RotateR) [0..]

part1Solution = last . part1 input1 <$> readFile "./data/Day21.txt"
part2Solution = head . part2 input2 <$> readFile "./data/Day21.txt"
part1 l s = scanl' doInstruction l $ fromSuccess $ parseInput s
part2 l s = scanr reverseDoInstruction l $ fromSuccess $ parseInput s

test1 = "abcde"
test2 = "decab"
testInstructions1 = "swap position 4 with position 0\nswap letter d with letter b\nreverse positions 0 through 4\nrotate left 1 step\nmove position 1 to position 4\nmove position 3 to position 0\nrotate based on position of letter b\nrotate based on position of letter d"
input1 = "abcdefgh"
input2 = "fbgdceah"
