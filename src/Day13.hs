module Day13 (part1, part2, test1, getTile, showMap, testMap, part2Solution, part1Solution) where

import           Data.Bits
import           Data.Graph.AStar
import qualified Data.HashSet     as H
import Data.List
import Data.Graph.Inductive.Query.BFS
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Hashable
import Data.Maybe

type Coord = (Integer, Integer)
data Tile = Wall | Open
  deriving (Eq,Ord,Show)

getWalkableAdjacents :: Integer -> Coord -> [Coord]
getWalkableAdjacents o (x,y)
  | getTile o (x,y) == Just Open = map fst $ filter (walkable . snd) $ map (\c -> (c,getTile o c)) [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
  | otherwise = []
  where
    walkable Nothing     = False
    walkable (Just Wall) = False
    walkable (Just Open) = True

getTile :: Integer -> Coord -> Maybe Tile
getTile o (x,y)
  | x < 0 || y < 0 = Nothing
  | even calc      = Just Open
  | otherwise      = Just Wall
  where
    calc = popCount $ x*x + 3*x + 2*x*y + y + y*y + o

getNode :: Coord -> Int
getNode = hash

getGraph :: Integer -> Integer -> Coord -> Gr Coord (Coord,Coord)
getGraph o d c@(xg,yg) = mkGraph
                      [(getNode (x,y), (x,y)) | x <- [xg-d..xg+d], y <- [yg-d..yg+d], withinRange o d (x,y) c]
                      [(getNode (x,y), getNode (x',y'),((x,y),(x',y')))
                      | x <- [xg-d..xg+d]
                      , y <- [yg-d..yg+d]
                      , x' <- [xg-d..xg+d]
                      , y' <- [yg-d..yg+d]
                      , (x',y') `elem` getWalkableAdjacents o (x,y)
                      , withinRange o d (x,y) c
                      , withinRange o d (x',y') c
                      ]

integralDist :: Coord -> Coord -> Integer
integralDist (x,y) (x',y') = floor $ sqrt (fromIntegral $ (x'-x)^2 + (y' - y)^2)

withinRange :: Integer -> Integer -> Coord -> Coord -> Bool
withinRange o m c1 c2 = case a of
  Nothing -> False
  (Just d) -> d <= m
  where
    a = part1 o c1 c2

part1 :: Integer -> Coord -> Coord -> Maybe Integer
part1 o s g = genericLength <$> aStar (H.fromList . getWalkableAdjacents o) (const . const 1) (integralDist g) (g ==) s

part2 :: Integer -> Integer -> [Coord]
part2 o d = mapMaybe (lab g) $ bfs (getNode (1,1)) g
  where g = getGraph o d (1,1)

part1Solution :: Maybe Integer
part1Solution = part1 input (1,1) (31,39)

part2Solution :: Integer
part2Solution = genericLength $ part2 input 50

showMap :: [[Maybe Tile]] -> [String]
showMap = map (map f)
  where
    f (Just Wall) = '#'
    f (Just Open) = '.'
    f Nothing     = 'x'

testMap :: [String]
testMap = [".#.####.##","..#..#...#","#....##...","###.#.###.",".##..#..#.","..##....#.","#...##.###"]
test1 :: Integer
test1 = 10
input :: Integer
input = 1350
