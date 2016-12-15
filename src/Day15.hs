module Day15 (part1,part2,test1) where

import           Data.List
import           Data.List.Split

type Position = Integer
type StartPosition = Integer
type Time = Integer
type NumPositions = Integer

data Disc = Disc NumPositions StartPosition
  deriving (Eq,Show)

discFunc :: Disc -> Time -> Position
discFunc (Disc np sp) t = (t + sp) `mod` np

timeLine :: [Disc] -> Time -> [Time]
timeLine ds t = [t..(t + genericLength ds - 1)]

part1 :: [Disc] -> Integer
part1 ds = genericLength $
          takeWhile (not . all (0 ==)) $
          chunksOf (length ds) $
          zipWith discFunc (cycle ds) $
          concatMap (timeLine ds) [1..]

part2 :: [Disc] -> Integer
part2= part1

part1Solution :: Integer
part1Solution = part1 input1

part2Solution :: Integer
part2Solution = part2 input2

test1 :: [Disc]
test1 = [Disc 5 4,Disc 2 1]

input1 :: [Disc]
input1 = [Disc 17 1,Disc 7 0,Disc 19 2,Disc 5 0,Disc 3 0,Disc 13 5]

input2 :: [Disc]
input2 = [Disc 17 1,Disc 7 0,Disc 19 2,Disc 5 0,Disc 3 0,Disc 13 5,Disc 11 0]
