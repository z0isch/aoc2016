module Day16 (part1,part2,test1,part2Solution) where

import Data.List.Split

dragon :: String -> String
dragon a = a ++ "0" ++ b
    where
      flipBit '0' = '1'
      flipBit '1' = '0'
      flipBit _ = error "Bad string"
      b = map flipBit $ reverse a

checksum :: String -> String
checksum x
  | odd (length y) = y
  | otherwise       = checksum y
  where
    y = foldMap (\[a,b] -> if a == b then "1" else "0") $ chunksOf 2 x

part1 i x = checksum $ take i $ head $ dropWhile ((< i) . length) $ iterate dragon x
part2 = part1

part1Solution = part1 272 "01110110101001000"
part2Solution = part2 35651584 "01110110101001000"
test1 = "10000"
