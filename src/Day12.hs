module Day12 (part1,part2,test1) where

import           Control.Applicative
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map
import           Text.Trifecta

type Register = Char

data Instruction = CpyI Integer Register | CpyR Register Register | Inc Register | Dec Register | JnzR Register Integer | JnzI Integer Integer
  deriving (Eq, Ord, Show)

type CurrentPosition = Int
data ProgramState = ProgramState CurrentPosition [Instruction] (Map Register Integer)
  deriving (Eq, Ord, Show)

startState :: [Instruction] -> ProgramState
startState is = ProgramState 0 is Map.empty
startState2 :: [Instruction] -> ProgramState
startState2 is = ProgramState 0 is $ Map.fromList [('c',1)]

instructionParser :: Parser Instruction
instructionParser = try cpyIParser <|> try cpyRParser <|> try incParser <|> try decParser <|> try jnzRParser <|> jnzIParser
  where
    cpyIParser = string "cpy " *> (CpyI <$> integer <*> letter)
    cpyRParser = string "cpy " *> (CpyR <$> letter <*> (space *> letter))
    incParser = string "inc " *> (Inc <$> letter)
    decParser = string "dec " *> (Dec <$> letter)
    jnzRParser = string "jnz " *> (JnzR <$> letter <*> (space *> integer ))
    jnzIParser = string "jnz " *> (JnzI <$> integer  <*> integer)

fromSuccess :: Result x -> x
fromSuccess (Success x) = x
fromSuccess (Failure x) = error (show x)

parseInput :: String -> [Instruction]
parseInput = fromSuccess . parseString (some (instructionParser <* skipOptional windowsNewLine)) mempty
  where windowsNewLine = const () <$ skipOptional newline <*> skipOptional (char '\r')

doNextInstruction :: ProgramState -> ProgramState
doNextInstruction ps@(ProgramState i instructions rMap)
  | i >= length instructions = ps
  | otherwise                = ProgramState (newI currInstruction) instructions (newMap currInstruction)
    where
      currInstruction = instructions !! i
      newI (JnzR r x)
        | currVal r > 0 = i + fromIntegral x
        | otherwise     = i+1
      newI (JnzI x y)
        | x > 0 = i + fromIntegral y
        | otherwise     = i+1
      newI _ = i+1
      currVal r = Map.findWithDefault 0 r rMap
      newMap (CpyI x r) = Map.insert r x rMap
      newMap (CpyR x y) = Map.insert y (currVal x) rMap
      newMap (Inc r)    = Map.insert r (currVal r + 1) rMap
      newMap (Dec r)    = Map.insert r (currVal r - 1) rMap
      newMap (JnzR _ _) = rMap
      newMap (JnzI _ _) = rMap

endOfInstruction :: ProgramState -> Bool
endOfInstruction (ProgramState i instructions _) = i >= length instructions

getRegister :: Register -> ProgramState -> Integer
getRegister r (ProgramState _ _ rMap) = Map.findWithDefault 0 r rMap

part1 :: Char -> String -> Integer
part1 c = getRegister c . last . takeWhile (not . endOfInstruction) . iterate doNextInstruction . startState . parseInput

part2 :: Char -> String -> Integer
part2 c = getRegister c . last . takeWhile (not . endOfInstruction) . iterate doNextInstruction . startState2 . parseInput

part1Solution :: IO Integer
part1Solution = part1 'a' <$> readFile "./data/Day12.txt"

part2Solution :: IO Integer
part2Solution = part2 'a' <$> readFile "./data/Day12.txt"

test1 :: String
test1="cpy 41 a\ninc a\ninc a\ndec a\njnz a 2\ndec a"
