module Day20 (part1,part2,test1, part1Solution,part2Solution,range1,numRestricted,restricted) where

import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Set      (Set)
import qualified Data.Set      as Set
import           Text.Trifecta

fromSuccess :: Result x -> x
fromSuccess (Success x) = x
fromSuccess (Failure x) = error (show x)

parseInput :: String -> Result [(Integer,Integer)]
parseInput = parseString (some (rangeParser <* skipOptional windowsNewLine)) mempty
  where windowsNewLine = const () <$ skipOptional newline <*> skipOptional (char '\r')

rangeParser :: Parser (Integer,Integer)
rangeParser = (,) <$> integer <*> (char '-' *> integer)

part1Solution :: IO Integer
part1Solution = part1 <$> readFile "./data/Day20.txt"
part1 :: String -> Integer
part1 s = head $ filter (\x -> not (any (inSet x) sets)) [0..]
  where
    inSet x (start,end) = x >= start && x <= end
    sets = fromSuccess $ parseInput s

numRestricted :: [(Integer, Integer)] -> Integer
numRestricted = getSum . foldMap (Sum . (\(x,y) -> y - x + 1)) .  restricted

restricted :: [(Integer, Integer)] -> Set (Integer, Integer)
restricted = foldl' addToSet Set.empty

addToSet :: Set (Integer, Integer) -> (Integer, Integer) -> Set (Integer, Integer)
addToSet s p
  | null merged = Set.insert new removeS
  | otherwise = addToSet removeS new
  where
    merged = mapMaybe (flatten p) (Set.toList s)
    removeS = foldl' (flip Set.delete) s merged
    new = foldl' (\(mi,mx) (x,y) -> (min x mi, max y mx)) p merged

flatten :: (Integer,Integer) -> (Integer, Integer) -> Maybe (Integer,Integer)
flatten p p'
  | cp p p' = Just p'
  | cp p' p = Just p'
  | otherwise = Nothing
  where
    cp (x,y) (x',y') = (x < x' && y >= x' && y <= y') ||
                        (x >= x' && x <= y' && y > y') ||
                        (x <= x' && y >= y')

part2Solution :: IO Integer
part2Solution = part2 (0,4294967295) <$> readFile "./data/Day20.txt"
part2 :: (Integer, Integer) -> String -> Integer
part2 (mi,mx) s = nums - numRestricted (fromSuccess $ parseInput s)
  where
    nums = mx - mi +1

test1 :: String
test1="5-8\n0-2\n4-7"
range1 :: (Integer, Integer)
range1 = (0,9)
