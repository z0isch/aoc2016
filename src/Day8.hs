module Day8 (part1, test1, bigScreen, smallScreen) where

import           Control.Applicative
import           Data.Foldable
import           Data.Matrix         (Matrix, getElem, mapCol, mapRow, matrix,
                                      ncols, nrows)
import           Data.Monoid
import           Text.Trifecta

data Instruction = Rect Int Int | RotateR Int Int | RotateC Int Int
  deriving (Eq,Show)

instructionParser :: Parser Instruction
instructionParser = try rectParser <|> try rotateRParser <|> rotateCParser
  where
    naturalInt = fromIntegral <$> natural
    cooordsParser d = d <$> naturalInt <*> (string "by " *> naturalInt)
    rotateCParser = string "rotate column x=" *> cooordsParser RotateC
    rotateRParser = string "rotate row y=" *> cooordsParser RotateR
    rectParser =  string "rect " *> (Rect <$> naturalInt <*> (char 'x' *> naturalInt))

parseInput :: String -> Result [Instruction]
parseInput = parseString (some (instructionParser <* skipOptional windowsNewLine)) mempty
  where windowsNewLine = const () <$ skipOptional newline <*> skipOptional (char '\r')

bigScreen :: Matrix Int
bigScreen = matrix 6 50 (const 0)

smallScreen :: Matrix Int
smallScreen = matrix 3 7 (const 0)

doInstruction :: Matrix Int -> Instruction -> Matrix Int
doInstruction m (Rect x y) = foldl' (flip (mapRow (\c v -> if c <= x then 1 else v))) m [1..y]
doInstruction m (RotateR x y) = mapRow (\c _ -> getElem (x+1) (wrapAround (ncols m) c y) m) (x+1) m
doInstruction m (RotateC x y) = mapCol (\r _ -> getElem (wrapAround (nrows m) r y) (x+1) m) (x+1) m

wrapAround :: Int -> Int -> Int -> Int
wrapAround e c i
  | d < 1 =  e + d
  | otherwise = d
  where d = c - i

fromSuccess :: Result x -> x
fromSuccess (Success x) = x
fromSuccess (Failure x) = error (show x)

part1 :: Matrix Int -> String -> Int
part1 m = getSum . foldMap Sum . foldl' doInstruction m . fromSuccess . parseInput

part1Solution :: IO Int
part1Solution = part1 bigScreen <$> readFile "./data/Day8.txt"

part2Solution :: IO (Matrix Char)
part2Solution = fmap (\x -> if x == 0 then ' ' else 'X' ) . foldl' doInstruction bigScreen . fromSuccess . parseInput <$> readFile "./data/Day8.txt"

test1 :: String
test1="rect 3x2\nrotate column x=1 by 1\nrotate row y=0 by 4\nrotate column x=1 by 1"
