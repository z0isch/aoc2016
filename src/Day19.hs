module Day19 (part1, part2, test1, input1) where

import           Control.Parallel.Strategies
import           Data.List
import qualified Data.Set                    as S

data Elf = Elf Integer Integer
  deriving (Eq, Show)

start :: Integer -> [Elf]
start i = map (`Elf` 1 ) [1..i]

accross :: Integer -> Integer -> Integer
accross l x = (x + n) `mod` l
  where
    n = l `div` 2

move2 :: [Elf] -> [Elf]
move2 [] = []
move2 [x] = [x]
move2 es = remaining2
  where
    n = length es `div` 2
    firstHalf = parMap rpar (f es) $ zip (reverse [0..genericLength es]) $ zip [0..] $ take n es
    gone = S.fromAscList $ map (fromIntegral . fst) firstHalf
    remaining = map snd firstHalf ++ parMap rpar (es !!) (filter (`S.notMember` gone) [n..length es-1])
    secondHalf = parMap rpar (f remaining) $ zip (reverse [0..genericLength remaining]) $ zip [genericLength firstHalf..] $ drop (length firstHalf) remaining
    gone2 = S.fromAscList $ map (fromIntegral . fst) secondHalf
    remaining2 = parMap rpar (map snd firstHalf !!) (filter (`S.notMember` gone2) [0..length firstHalf-1]) ++ map snd secondHalf
    f xs (l,(i,e)) = let p = accross l i + (genericLength xs - l)
                     in (p,makeSwap e (xs !! fromIntegral p))

move :: [Elf] -> [Elf]
move []  = []
move [x] = [x]
move es
  | even (length es) = map (uncurry makeSwap) paired
  | otherwise        = let evens = move (init es)
                       in tail evens ++ [makeSwap (last es) (head evens)]
  where
    everyOther ::  [a] -> ([a],[a])
    everyOther []       = ([],[])
    everyOther [x]      = ([x],[])
    everyOther (x:y:xs) = let (xp,yp) = everyOther xs in (x:xp,y:yp)
    paired = uncurry zip $ everyOther es

makeSwap :: Elf -> Elf -> Elf
makeSwap (Elf x xi) (Elf _ yi) = Elf x (xi + yi)

getElfNum (Elf i _) = i
part1 i = getElfNum $ head $ head $ dropWhile ((>1) . length) $ iterate move (start i)
part2 i = getElfNum $ head $ head $ dropWhile ((>1) . length) $ iterate move2 (start i)

test1,input1 :: Integer
test1 = 5
input1 = 3005290
