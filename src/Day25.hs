module Day25 (part1, part1Solution) where

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
                   | OutR Register
                   | OutI Integer
                   | Skip Instruction
  deriving (Eq, Ord, Show)

type CurrentPosition = Int
data ProgramState = ProgramState CurrentPosition (Seq Instruction) (Map Register Integer) (Seq Integer)
  deriving (Eq, Ord, Show)

startState :: Map Register Integer -> [Instruction] -> ProgramState
startState m is = ProgramState 0 (S.fromList is) m S.empty

instructionParser :: Parser Instruction
instructionParser = try cpyIParser
                    <|> try cpyRParser
                    <|> try incParser
                    <|> try decParser
                    <|> try jnzRParser
                    <|> try jnzR'Parser
                    <|> try tglParser
                    <|> try outIParser
                    <|> try outRParser
                    <|> jnzIParser
  where
    cpyIParser = string "cpy " *> (CpyI <$> integer <*> letter)
    cpyRParser = string "cpy " *> (CpyR <$> letter <*> (space *> letter))
    incParser = string "inc " *> (Inc <$> letter)
    decParser = string "dec " *> (Dec <$> letter)
    jnzRParser = string "jnz " *> (JnzR <$> letter <*> (space *> integer ))
    jnzR'Parser = string "jnz " *> (JnzR' <$> integer <*> letter)
    jnzIParser = string "jnz " *> (JnzI <$> integer  <*> integer)
    outIParser = string "out " *> (OutI <$> integer)
    outRParser = string "out " *> (OutR <$> letter)
    tglParser = string "tgl " *> (Tgl <$> letter)

fromSuccess :: Result x -> x
fromSuccess (Success x) = x
fromSuccess (Failure x) = error (show x)

parseInput :: String -> [Instruction]
parseInput = fromSuccess . parseString (some (instructionParser <* skipOptional windowsNewLine)) mempty
  where windowsNewLine = const () <$ skipOptional newline <*> skipOptional (char '\r')

doNextInstruction :: ProgramState -> ProgramState
doNextInstruction ps@(ProgramState i instructions rMap output)
  | endOfInstruction ps = ps
  | otherwise           = ProgramState (newI currInstruction)
                                       (newInstructions currInstruction)
                                       (newMap currInstruction)
                                       (newOutput currInstruction)
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
      newOutput (OutR r) = output S.|> currVal r
      newOutput (OutI int) = output S.|> int
      newOutput _ = output

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
tglInstruction (OutR t) = Inc t
tglInstruction i@(OutI _) = Skip i
tglInstruction (Skip i) = case n of
                            (Skip ins) -> Skip ins
                            _ -> n
  where n = tglInstruction i

endOfInstruction :: ProgramState -> Bool
endOfInstruction (ProgramState i instructions _ _) = i >= S.length instructions

heursticLength :: Int
heursticLength = 10

outputLongEnough :: ProgramState -> Bool
outputLongEnough (ProgramState _ _ _ output) = S.length output >= heursticLength

anyOutput :: ProgramState -> Bool
anyOutput (ProgramState _ _ _ output) = not $ S.null output

isLookingLikeSignal :: ProgramState -> Bool
isLookingLikeSignal (ProgramState _ _ _ output) = isSignal
  where
    isSignal = and $ S.zipWith (==) clockSignal output
    clockSignal = S.fromList $ take heursticLength $ cycle [0,1]

isClockSignal :: ProgramState -> Bool
isClockSignal ps = isLookingLikeSignal ps && outputLongEnough ps

clockSignalTry :: [Instruction] -> Map Register Integer -> (Map Register Integer, ProgramState)
clockSignalTry i m = (m,part)
  where part = head $
            dropWhile (\s -> isLookingLikeSignal s && not (outputLongEnough s)) $
            dropWhile (not . anyOutput) $
            iterate doNextInstruction $
            startState m i

part1 :: String -> Map Register Integer
part1 i = fst $ head $
        dropWhile (not . isClockSignal . snd) $
        map (clockSignalTry (parseInput i) . (\x -> M.fromList [('a',x)])) [1..]
part1Solution :: IO (Map Register Integer)
part1Solution = part1 <$> readFile "./data/Day25.txt"
