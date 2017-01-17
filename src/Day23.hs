module Day23 (part1,part2,test1,part1Solution, part2Solution) where

import           Control.Applicative
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as M
import           Data.Sequence       (Seq)
import qualified Data.Sequence       as S
import           Text.Trifecta

type Register = Char

data Instruction = CpyI Integer Register
                   | CpyR Register Register
                   | Inc Register
                   | Dec Register
                   | JnzR Register Integer
                   | JnzR' Integer Register
                   | JnzR'' Register Register
                   | JnzI Integer Integer
                   | Tgl Register
                   | Skip Instruction
  deriving (Eq, Ord, Show)

type CurrentPosition = Int
data ProgramState = ProgramState CurrentPosition (Seq Instruction) (Map Register Integer)
  deriving (Eq, Ord, Show)

startState :: Map Register Integer -> [Instruction] -> ProgramState
startState m is = ProgramState 0 (S.fromList is) m

instructionParser :: Parser Instruction
instructionParser = try cpyIParser <|> try cpyRParser <|> try incParser <|> try decParser <|> try jnzRParser <|> try jnzR'Parser <|> try tglParser <|> jnzIParser
  where
    cpyIParser = string "cpy " *> (CpyI <$> integer <*> letter)
    cpyRParser = string "cpy " *> (CpyR <$> letter <*> (space *> letter))
    incParser = string "inc " *> (Inc <$> letter)
    decParser = string "dec " *> (Dec <$> letter)
    jnzRParser = string "jnz " *> (JnzR <$> letter <*> (space *> integer ))
    jnzR'Parser = string "jnz " *> (JnzR' <$> integer <*> letter)
    jnzIParser = string "jnz " *> (JnzI <$> integer  <*> integer)
    tglParser = string "tgl " *> (Tgl <$> letter)

fromSuccess :: Result x -> x
fromSuccess (Success x) = x
fromSuccess (Failure x) = error (show x)

parseInput :: String -> [Instruction]
parseInput = fromSuccess . parseString (some (instructionParser <* skipOptional windowsNewLine)) mempty
  where windowsNewLine = const () <$ skipOptional newline <*> skipOptional (char '\r')

doNextInstruction :: ProgramState -> ProgramState
doNextInstruction ps@(ProgramState i instructions rMap)
  | endOfInstruction ps = ps
  | otherwise           = ProgramState (newI currInstruction) (newInstructions currInstruction) (newMap currInstruction)
    where
      currInstruction = S.index instructions i
      newI (JnzR' x r)
        | x > 0 = i + fromIntegral (currVal r)
        | otherwise     = i+1
      newI (JnzR'' r1 r2)
        | currVal r1 > 0 = i + fromIntegral (currVal r2)
        | otherwise     = i+1
      newI (JnzR r x)
        | currVal r > 0 = i + fromIntegral x
        | otherwise     = i+1
      newI (JnzI x y)
        | x > 0 = i + fromIntegral y
        | otherwise     = i+1
      newI _ = i+1
      currVal r = M.findWithDefault 0 r rMap
      newInstructions (Tgl r) = S.adjust tglInstruction instructionToTgl instructions
        where
          instructionToTgl = i + fromIntegral (currVal r)
      newInstructions _ = instructions
      newMap (CpyI x r) = M.insert r x rMap
      newMap (CpyR x y) = M.insert y (currVal x) rMap
      newMap (Inc r)    = M.insert r (currVal r + 1) rMap
      newMap (Dec r)    = M.insert r (currVal r - 1) rMap
      newMap _ = rMap

tglInstruction :: Instruction -> Instruction
tglInstruction (CpyI t1 t2) = JnzR' t1 t2
tglInstruction (CpyR t1 t2) = JnzR'' t1 t2
tglInstruction (Inc t) = Dec t
tglInstruction (Dec t) = Inc t
tglInstruction i@(JnzR _ _) = Skip i
tglInstruction (JnzR' t1 t2) = CpyI t1 t2
tglInstruction (JnzR'' t1 t2) = CpyR t1 t2
tglInstruction i@(JnzI _ _) = Skip i
tglInstruction (Tgl t) = Inc t
tglInstruction (Skip i) = case n of
                            (Skip ins) -> Skip ins
                            _ -> n
  where n = tglInstruction i

endOfInstruction :: ProgramState -> Bool
endOfInstruction (ProgramState i instructions _) = i >= S.length instructions

getRegister :: Register -> ProgramState -> Integer
getRegister r (ProgramState _ _ rMap) = M.findWithDefault 0 r rMap

part1 :: Map Register Integer -> Char -> String -> Integer
part1 m c = getRegister c . last . takeWhile (not . endOfInstruction) . iterate doNextInstruction . startState m . parseInput
part1Solution :: IO Integer
part1Solution = part1 (M.fromList [('a',7)]) 'a' <$> readFile "./data/Day23.txt"

part2 :: Map Register Integer -> Char -> String -> Integer
part2 = part1
part2Solution :: IO Integer
part2Solution = part2 (M.fromList [('a',12)]) 'a' <$> readFile "./data/Day23.txt"

test1 :: String
test1 = "cpy 2 a\ntgl a\ntgl a\ntgl a\ncpy 1 a\ndec a\ndec a"
