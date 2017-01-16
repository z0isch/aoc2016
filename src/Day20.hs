module Day20 (part1,part2,test1, part1Solution,part2Solution) where

import           Data.List
import           Data.Maybe
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

part1Solution = part1 <$> readFile "./data/Day20.txt"
part1 s = head $ filter (\x -> not (any (inSet x) sets)) [0..]
  where
    inSet x (start,end) = x >= start && x <= end
    sets = fromSuccess $ parseInput s

flatten :: (Integer,Integer) -> (Integer, Integer) -> Maybe (Integer,Integer)
flatten (st,end) (st',end')
  | st < st' && end > st' && end <= end' = Just (st,end')
  | st >= st' && st < end' && end > end' = Just (st',end)
  | st < st' && end > end' = Just (st,end)
  | otherwise = Nothing

t1 = fromSuccess $ parseInput test1

part2Solution = part2 <$> readFile "./data/Day20.txt"
part2 s = genericLength $ nub $ concatMap (uncurry enumFromTo) sets
  where
    sets =  fromSuccess $ parseInput s

test1="5-8\n0-2\n4-7"
