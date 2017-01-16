{-# LANGUAGE DeriveGeneric #-}
module Day22 (part2Print, part1,part2,test1,test2,test3,part2Solution,profilePart2, moves, moves', heuristic,heuristic') where

import           Control.Monad
import           Control.Parallel.Strategies
import           Data.Graph.AStar
import           Data.Hashable
import           Data.HashSet                (HashSet)
import qualified Data.HashSet                as HS
import           Data.List
import qualified Data.Map.Strict             as Map
import           Data.Maybe
import           GHC.Generics                (Generic)
import           Safe
import           Text.Trifecta

type NodeCoord = (Integer,Integer)

data NodeState = NodeState
  { _nodeStateGoal  :: Node
  , _nodeStateNodes :: Map.Map NodeCoord Node
  , _stepsTaken     :: Integer
  }
  deriving (Eq,Show,Ord,Generic)
instance Hashable NodeState where
  hashWithSalt salt (NodeState goal ns steps) = mapHashed
    where
      mapHashed = Map.foldl' step (salt `hashWithSalt` goal) ns `hashWithSalt` steps
      step n a  = n `hashWithSalt` a

data Node = Node
  { _nodeCoord :: NodeCoord
  , _nodeSize  :: Integer
  , _nodeUsed  :: Integer
  , _nodeData  :: [(Integer,Integer)]
  }
  deriving (Eq,Show,Ord,Generic)
instance Hashable Node

parseInput :: String -> NodeState
parseInput = fromSuccess . parseString (nodeStateMaker <$> some nodeParser <* skipOptional windowsNewLine) mempty
  where
    nodeStateMaker ns = NodeState (goal nodes) nodes 0
      where nodes = Map.fromList $ map (\n -> (_nodeCoord n,n)) ns
    goal ns = ns Map.! (foldr (\(Node (x,y) _ _ _) m -> if y == 0 then max m x else m) 0 ns,0)
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
isViable (Node c1 _ u1 _) (Node c2 s2 u2 _)
  | c1 == c2  = False
  | otherwise = u1 > 0 && u1 <= a2
  where a2 = s2 - u2

distance :: NodeCoord -> NodeCoord -> Integer
distance (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)

adjacents :: NodeState -> Node -> [Node]
adjacents (NodeState _ ns _) (Node (x,y) _ _ _) = mapMaybe (`Map.lookup` ns) [(x,y-1),(x,y+1),(x-1,y),(x+1,y)]

moveData :: (Node,Node) -> (Node,Node)
moveData (Node c1 s1 u1 d1, Node c2 s2 u2 d2) = (Node c1 s1 0 [], Node c2 s2 (u1+u2) (d2 ++ d1))

unViableNodes :: [Node] -> HashSet Node
unViableNodes ns = HS.fromList ns `HS.difference` found
  where found = foldl' (\h (x,y) ->  HS.insert y $ HS.insert x h) HS.empty $ viables ns

moves' :: NodeState -> HashSet NodeState
moves' state@(NodeState goal ns steps)
  | goalCanFit = HS.fromList newStates
  | otherwise = HS.empty
  where
    goalCanFit = not $ Map.null $ Map.filter (isViable goal) ns
    newStates = map (newState . moveData) viableAdjacents
    newState c = NodeState (newGoal c) (removeUnviable $ adjustedNodes c) (steps+1)
    newGoal (a,b) = if _nodeCoord a == _nodeCoord goal then b else goal
    adjustedNodes (a,b) = Map.adjust (const b) (_nodeCoord b) $ Map.adjust (const a) (_nodeCoord a) ns
    viableAdjacents = filter (uncurry isViable) $ concatMap (\n -> zip (repeat n) $ adjacents state n) $ Map.elems ns
    removeUnviable m = HS.foldl' (\s n -> Map.delete (_nodeCoord n) s) m $ unViableNodes (Map.elems m)

moves :: NodeState -> HashSet NodeState
moves state@(NodeState goal ns steps)
  | goalCanFit = HS.fromList newStates
  | otherwise = HS.empty
  where
    goalCanFit = not $ Map.null $ Map.filter (isViable goal) ns
    newStates = parMap rpar (newState . moveData) viableAdjacents
    newState c = NodeState (newGoal c) (adjustedNodes c) (steps+1)
    newGoal (a,b) = if _nodeCoord a == _nodeCoord goal then b else goal
    adjustedNodes (a,b) = Map.adjust (const b) (_nodeCoord b) $ Map.adjust (const a) (_nodeCoord a) ns
    viableAdjacents = filter (uncurry isViable) $ concat $ parMap rpar (\n -> zip (repeat n) $ adjacents state n) $ Map.elems ns

isGoal :: NodeState -> Bool
isGoal (NodeState goal _ _) = _nodeCoord goal == (0,0)

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
numberOfMovesToMove :: NodeState -> Node -> Integer
numberOfMovesToMove state start = go (HS.singleton start) start
  where
    go :: HashSet Node -> Node -> Integer
    go seen n
      | anyViable = 1
      | otherwise = 1 + safeMin (map (go (HS.insert n seen)) unseen)
      where
        adjs = adjacents state n
        anyViable = any (uncurry isViable) $ zip (repeat n) adjs
        unseen = filter (not . (`HS.member` seen)) adjs
        safeMin [] = 99999
        safeMin xs = minimum xs

heuristic'' :: NodeState -> Integer
heuristic'' state@(NodeState goal ns _)
  | isGoal state = 0
  | otherwise = distanceToGoal + fromIntegral numMoveMove
  where
    goalNodeCoord = _nodeCoord goal
    distanceToGoal = distance (0,0) goalNodeCoord
    numMoveMove = minimum $ map (minimum . parMap rpar (numberOfMovesToMove state . getNode)) goalPaths
    getNode = (Map.!) ns
    goalPaths = pathsToEnd goalNodeCoord

heuristic' :: NodeState -> Integer
heuristic' state@(NodeState goal ns _)
  | isGoal state = 0
  | otherwise = distanceToGoal + closestViable + fromIntegral clearPathToGoal
  where
    goalNodeCoord = _nodeCoord goal
    clearPathToGoal = minimum $ map (nonViables . map getNode) $
                      filter (all (`Map.member` ns)) $
                      pathsToEnd goalNodeCoord
    nonViables nodes = length nodes - length (filter (isViable goal) nodes)
    closestViable = minimum $
                      map (uncurry max) $
                      filter (\(x,y) -> x == 0 || y == 0) $
                      map (\(x,y) -> (distance goalNodeCoord (_nodeCoord x),distance goalNodeCoord (_nodeCoord y))) $
                      viables $ Map.elems ns
    distanceToGoal = distance (0,0) goalNodeCoord
    getNode = (Map.!) ns

heuristic :: NodeState -> Integer
heuristic state@(NodeState goal ns _)
  | isGoal state = 0
  | otherwise = distanceToGoal + fromIntegral clearPathToGoal
  where
    goalNodeCoord = _nodeCoord goal
    getNode = (Map.!) ns
    clearPathToGoal = minimum $ map (nonViables . map getNode) $ pathsToEnd goalNodeCoord
    nonViables nodes = length nodes - length (filter (isViable goal) nodes)
    distanceToGoal = distance (0,0) goalNodeCoord

viables :: [Node] -> [(Node,Node)]
viables ns = [(a,b) | a <- ns, b <- ns, isViable a b]

part1 :: String -> Int
part1 = length . viables . Map.elems . _nodeStateNodes . parseInput

printHeuristic :: NodeState -> IO Integer
printHeuristic s = do
  let h = heuristic' s
  print h
  return h

printMoves :: NodeState -> IO (HashSet NodeState)
printMoves s = do
  let mvs = moves' s
      steps = _stepsTaken <$> headMay (HS.toList mvs)
  print steps
  return mvs

part2 :: String -> Maybe Int
part2 s = length <$> aStar moves (const $ const 1) heuristic' isGoal (parseInput s)

part2Print :: String -> IO Int
part2Print s = length <$> aStarM printMoves (\_ _ -> return 1) printHeuristic (return . isGoal) (return $ parseInput s)

profilePart2 :: (NodeState -> HashSet NodeState) -> (NodeState -> Integer) -> String -> Maybe Int
profilePart2 m h s = length <$> aStar m (const $ const 1) h isGoal (parseInput s)

part2Solution :: IO (Maybe Int)
part2Solution = part2 <$> readFile "./data/Day22.txt"

test1 :: String
test1 = "/dev/grid/node-x0-y0   10T    8T     2T   80%\n/dev/grid/node-x0-y1   11T    6T     5T   54%\n/dev/grid/node-x0-y2   32T   28T     4T   87%\n/dev/grid/node-x1-y0    9T    7T     2T   77%\n/dev/grid/node-x1-y1    8T    0T     8T    0%\n/dev/grid/node-x1-y2   11T    7T     4T   63%\n/dev/grid/node-x2-y0   10T    6T     4T   60%\n/dev/grid/node-x2-y1    9T    8T     1T   88%\n/dev/grid/node-x2-y2    9T    6T     3T   66%"
test2 :: String
test2 = "/dev/grid/node-x0-y0   10T    8T     2T   80%\n/dev/grid/node-x0-y1   11T    6T     5T   54%\n/dev/grid/node-x0-y2   32T   28T     4T   87%\n/dev/grid/node-x0-y3   32T   28T     4T   87%\n/dev/grid/node-x1-y0    9T    7T     2T   77%\n/dev/grid/node-x1-y1    8T    0T     8T    0%\n/dev/grid/node-x1-y2   11T    7T     4T   63%\n/dev/grid/node-x1-y3   11T    7T     4T   63%\n/dev/grid/node-x2-y0   10T    6T     4T   60%\n/dev/grid/node-x2-y1    9T    8T     1T   88%\n/dev/grid/node-x2-y2    9T    6T     3T   66%\n/dev/grid/node-x2-y3    9T    6T     3T   66%\n/dev/grid/node-x3-y0   10T    6T     4T   60%\n/dev/grid/node-x3-y1    9T    8T     1T   88%\n/dev/grid/node-x3-y2    9T    6T     3T   66%\n/dev/grid/node-x3-y3    9T    6T     3T   66%"
test3 :: String
test3 = "/dev/grid/node-x0-y0   10T    8T     2T   80%\n/dev/grid/node-x0-y1   11T    6T     5T   54%\n/dev/grid/node-x0-y2   32T   28T     4T   87%\n/dev/grid/node-x0-y3   32T   28T     4T   87%\n/dev/grid/node-x0-y4   32T   28T     4T   87%\n/dev/grid/node-x1-y0    9T    7T     2T   77%\n/dev/grid/node-x1-y1    8T    0T     8T    0%\n/dev/grid/node-x1-y2   11T    7T     4T   63%\n/dev/grid/node-x1-y3   11T    7T     4T   63%\n/dev/grid/node-x1-y4   11T    7T     4T   63%\n/dev/grid/node-x2-y0   10T    6T     4T   60%\n/dev/grid/node-x2-y1    9T    8T     1T   88%\n/dev/grid/node-x2-y2    9T    6T     3T   66%\n/dev/grid/node-x2-y3    9T    6T     3T   66%\n/dev/grid/node-x2-y4    9T    6T     3T   66%\n/dev/grid/node-x3-y0   10T    6T     4T   60%\n/dev/grid/node-x3-y1    9T    8T     1T   88%\n/dev/grid/node-x3-y2    9T    6T     3T   66%\n/dev/grid/node-x3-y3    9T    6T     3T   66%\n/dev/grid/node-x3-y4    9T    6T     3T   66%\n/dev/grid/node-x4-y0   10T    6T     4T   60%\n/dev/grid/node-x4-y1    9T    8T     1T   88%\n/dev/grid/node-x4-y2    9T    6T     3T   66%\n/dev/grid/node-x4-y3    9T    6T     3T   66%\n/dev/grid/node-x4-y4    9T    6T     3T   66%"
