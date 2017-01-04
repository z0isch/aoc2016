{-# LANGUAGE DeriveGeneric #-}
module Day22 (part1,part2,test1,part2Solution) where

import           Control.Monad
import           Text.Trifecta
import qualified Data.Map.Strict as Map
import           Data.Graph.AStar
import           Data.Hashable
import  Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import           GHC.Generics     (Generic)

type NodeCoord = (Integer,Integer)

data NodeState = NodeState
  { _nodeStateGoal :: NodeCoord
  , _nodeStateNodes :: [Node]
  }
  deriving (Eq,Show,Ord,Generic)
instance Hashable NodeState

data Node = Node
  { _nodeCoord :: NodeCoord
  , _nodeSize :: Integer
  , _nodeUsed :: Integer
  , _nodeData :: [(Integer,Integer)]
  }
  deriving (Eq,Show,Ord,Generic)
instance Hashable Node

parseInput :: String -> NodeState
parseInput = fromSuccess . parseString ((\ns -> NodeState (goal ns) ns) <$> some nodeParser <* skipOptional windowsNewLine) mempty
  where
    goal ns = (foldr (\(Node (x,y) _ _ _) m -> if y == 0 then max m x else m) 0 ns,0)
    fromSuccess (Success x) = x
    fromSuccess (Failure x) = error (show x)
    windowsNewLine = void $ skipOptional newline <* skipOptional (char '\r')

nodeParser :: Parser Node
nodeParser = (\c s u -> Node c s u [c]) <$>
            coordParser <*>
            numParser 'T' <*>
            numParser 'T' <*
            numParser 'T' <*
            numParser '%'
  where
    coordParser = string "/dev/grid/node-x" *> ((,) <$> integer <*> (string "-y" *> integer))
    numParser c = whiteSpace *> integer <* (char c <* whiteSpace)

isViable :: Node -> Node -> Bool
isViable (Node _ _ u1 _) (Node _ s2 u2 _) = u1 > 0 && u1 <= a2
  where a2 = s2 - u2

distance :: NodeCoord -> NodeCoord -> Integer
distance (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)

isAdjacent :: Node -> Node -> Bool
isAdjacent a b = distance (_nodeCoord a) (_nodeCoord b) == 1

moveData :: (Node,Node) -> (Node,Node)
moveData (Node c1 s1 u1 d1, Node c2 s2 u2 d2) = (Node c1 s1 0 [], Node c2 s2 (u1+u2) (d2 ++ d1))

moves :: NodeState -> HashSet NodeState
moves state@(NodeState goal ns) = HS.fromList $ filter viableState newStates
  where
    newStates = map (NodeState goal . Map.elems . adjustState . moveData) viableAdjacents
    viableState (NodeState _ nodes) = any (\(a,b) -> _nodeCoord a == _nodeCoord (goalNode state) || _nodeCoord b == _nodeCoord (goalNode state)) $ viables nodes
    nodeState = Map.fromList $ map (\n -> (_nodeCoord n, n)) ns
    adjustState (a,b) = Map.adjust (const b) (_nodeCoord b) $ Map.adjust (const a) (_nodeCoord a) nodeState
    viableAdjacents = [(a,b) | a <- ns, b <- ns, a /= b, isViable a b, isAdjacent a b]

isGoal :: NodeState -> Bool
isGoal (NodeState goal ns) = any (\(Node (x,y) _ _ ds) -> x == 0 && y ==0 && goal `elem` ds) ns

pathsToEnd :: NodeCoord -> [[NodeCoord]]
pathsToEnd nc = map init $ last $ takeWhile (not.null) $ iterate (concatMap f) (f [nc])
  where
    f []     = []
    f (n:ns) = [nn:n:ns | nn <- nodesTowardEnd n]

nodesTowardEnd :: NodeCoord -> [NodeCoord]
nodesTowardEnd c@(xc,yc) = [(x,y) | x <- [xc+1,xc,xc-1]
                                  , y <- [yc+1,yc,yc-1]
                                  , distance (x,y) c == 1
                                  , distance (0,0) (x,y) < distance (0,0) c
                           ]

goalNode :: NodeState -> Node
goalNode (NodeState goal ns) = head $ filter (\(Node _ _ _ ds) -> goal `elem` ds) ns

heurstic :: NodeState -> Integer
heurstic state@(NodeState _ ns)
  | isGoal state = 0
  | otherwise = distanceToGoal + fromIntegral pathToGoal
  where
    getNode c = head $ filter ((==) c . _nodeCoord) ns
    pathToGoal = minimum $ map (nonViables . map getNode) $ pathsToEnd (_nodeCoord $ goalNode state)
    nonViables nodes = length nodes - length (filter (isViable $ goalNode state) nodes)
    distanceToGoal = distance (0,0) $ _nodeCoord $ goalNode state

viables :: [Node] -> [(Node,Node)]
viables ns = [(a,b) | a <- ns, b <- ns, a /= b, isViable a b]

part1 :: String -> Int
part1 = length . viables . _nodeStateNodes . parseInput

part2 :: String -> Maybe Int
part2 s = length <$> aStar moves (const $ const 1) heurstic isGoal (parseInput s)

part2Solution :: IO (Maybe Int)
part2Solution = part2 <$> readFile "./data/Day22.txt"

test1 :: String
test1 = "/dev/grid/node-x0-y0   10T    8T     2T   80%\n/dev/grid/node-x0-y1   11T    6T     5T   54%\n/dev/grid/node-x0-y2   32T   28T     4T   87%\n/dev/grid/node-x1-y0    9T    7T     2T   77%\n/dev/grid/node-x1-y1    8T    0T     8T    0%\n/dev/grid/node-x1-y2   11T    7T     4T   63%\n/dev/grid/node-x2-y0   10T    6T     4T   60%\n/dev/grid/node-x2-y1    9T    8T     1T   88%\n/dev/grid/node-x2-y2    9T    6T     3T   66%"
test2 :: String
test2 = "/dev/grid/node-x0-y0   10T    8T     2T   80%\n/dev/grid/node-x0-y1   11T    6T     5T   54%\n/dev/grid/node-x0-y2   32T   28T     4T   87%\n/dev/grid/node-x0-y3   32T   28T     4T   87%\n/dev/grid/node-x1-y0    9T    7T     2T   77%\n/dev/grid/node-x1-y1    8T    0T     8T    0%\n/dev/grid/node-x1-y2   11T    7T     4T   63%\n/dev/grid/node-x1-y3   11T    7T     4T   63%\n/dev/grid/node-x2-y0   10T    6T     4T   60%\n/dev/grid/node-x2-y1    9T    8T     1T   88%\n/dev/grid/node-x2-y2    9T    6T     3T   66%\n/dev/grid/node-x2-y3    9T    6T     3T   66%\n/dev/grid/node-x3-y0   10T    6T     4T   60%\n/dev/grid/node-x3-y1    9T    8T     1T   88%\n/dev/grid/node-x3-y2    9T    6T     3T   66%\n/dev/grid/node-x3-y3    9T    6T     3T   66%"
