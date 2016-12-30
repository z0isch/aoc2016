module Day17 (part1, part2, input1, test1, test2, test3) where

import           Control.Parallel.Strategies
import           Data.Hash.MD5

type Point = (Int,Int)

data Direction = U | D | L | R
  deriving (Eq, Show)

data MapState = MapState [Direction] Point
  deriving (Eq,Show)

startState :: MapState
startState = MapState [] (0,0)

isEndState :: MapState -> Bool
isEndState (MapState _ (3,3)) = True
isEndState _                  = False

directionString :: MapState -> String
directionString (MapState ds _) = concatMap show ds

validDoors :: String -> MapState -> [Direction]
validDoors i ms@(MapState _ p) = map fst $
                                filter (\(d,c) -> isValid p d && isOpen c) $
                                zip [U,D,L,R] $ take 4 $
                                md5s $ Str $
                                i ++ directionString ms

nextStates :: String -> MapState -> [MapState]
nextStates i ms@(MapState ds p)
  | isEndState ms = []
  | otherwise     = map (\d -> MapState (ds++[d]) (nextPoint p d)) $ validDoors i ms

nextPoint :: Point -> Direction -> Point
nextPoint (x,y) U = (x,y-1)
nextPoint (x,y) D = (x,y+1)
nextPoint (x,y) L = (x-1,y)
nextPoint (x,y) R = (x+1,y)

isValid :: Point -> Direction -> Bool
isValid (_,y) U = y > 0
isValid (_,y) D = y < 3
isValid (x,_) L = x > 0
isValid (x,_) R = x < 3

isOpen :: Char -> Bool
isOpen c
  | c `elem` ['b'..'f'] = True
  | otherwise           = False

bfs :: String -> [MapState]
bfs i = concat $ takeWhile (not. null) $ iterate (concat . parMap rseq (nextStates i)) [startState]

part1 :: String -> String
part1 i = directionString $ head $ dropWhile (not . isEndState) $ bfs i
part2 :: String -> Int
part2 i =  length $ directionString $ head $ dropWhile (not . isEndState) $ reverse $ bfs i

part1Solution :: String
part1Solution = part1 input1
part2Solution :: Int
part2Solution = part2 input1

input1,test1,test2,test3 :: String
input1 = "gdjjyniy"
test1 = "ihgpwlah"
test2 = "kglvqrro"
test3 = "ulqzkmiv"
