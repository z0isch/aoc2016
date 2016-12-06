module Day6 (part1,test1,part2) where

import           Data.List
import qualified Data.Map.Strict as Map

orderedHistMap :: ((Char, Integer) -> Char -> Integer -> (Char, Integer)) -> [String] -> String
orderedHistMap f = map snd . Map.toAscList . fmap (fst . Map.foldlWithKey' f (' ',0)) . histMap

histMap :: [String] -> Map.Map Integer (Map.Map Char Integer)
histMap = foldl' (\m cs -> foldl' (\m' (i,c) -> Map.alter (alterF c) i m') m (zip [0..] cs)) Map.empty
  where
    alterF c Nothing  = Just $ Map.singleton c 1
    alterF c (Just m) = Just $ Map.alter innerAlter c m
    innerAlter Nothing  = Just 1
    innerAlter (Just x) = Just (x+1)

part1Solution :: IO String
part1Solution = part1 . lines <$> readFile "./data/Day6.txt"

part2Solution :: IO String
part2Solution = part2 . lines <$> readFile "./data/Day6.txt"

part1 :: [String] -> String
part1 =  orderedHistMap countFold
  where
    countFold (c,ct) c' ct'
      | c == ' '   = (c',ct')
      | ct' > ct  = (c',ct')
      | otherwise = (c,ct)

part2 :: [String] -> String
part2 = orderedHistMap countFold
  where
    countFold (c,ct) c' ct'
      | c == ' '   = (c',ct')
      | ct' <= ct  = (c',ct')
      | otherwise = (c,ct)

test1="eedadn\ndrvtee\neandsr\nraavrd\natevrs\ntsrnev\nsdttsa\nrasrtv\nnssdts\nntnada\nsvetve\ntesnvt\nvntsnd\nvrdear\ndvrsen\nenarar"
