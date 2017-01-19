{-# LANGUAGE DeriveGeneric #-}
module Day22 (part2Print, part1,part2,test1,test2,test3,part2Solution,profilePart2, moves, heuristic) where

import           Control.Monad
import           Data.Graph.AStar
import           Data.Hashable
import           Data.HashSet     (HashSet)
import qualified Data.HashSet     as HS
import           Data.List
import qualified Data.Map.Strict  as Map
import           Data.Maybe
import           GHC.Generics     (Generic)
import           Safe
import           Text.Trifecta

type NodeCoord = (Integer,Integer)

data NodeState = NodeState
  { _nodeStateGoal  :: Node
  , _nodeStateEmpty :: Node
  , _nodeStateNodes :: Map.Map NodeCoord Node
  , _stepsTaken     :: Integer
  }
  deriving (Eq,Show,Ord,Generic)
instance Hashable NodeState where
  hashWithSalt salt (NodeState goal empty ns _) = mapHashed `hashWithSalt` empty
    where
      mapHashed = Map.foldlWithKey' step (salt `hashWithSalt` goal) ns
      step n a b = n `hashWithSalt` a `hashWithSalt` b

data Node = Node
  { _nodeCoord :: NodeCoord
  , _nodeSize  :: Integer
  , _nodeUsed  :: Integer
  }
  deriving (Eq,Show,Ord,Generic)
instance Hashable Node

parseInput :: String -> NodeState
parseInput = fromSuccess . parseString (nodeStateMaker <$> some nodeParser <* skipOptional windowsNewLine) mempty
  where
    nodeStateMaker ns = NodeState (goal nodes) (empty nodes) nodes 0
      where nodes = Map.fromList $ map (\n -> (_nodeCoord n,n)) ns
    goal ns = ns Map.! (foldr (\(Node (x,y) _ _) m -> if y == 0 then max m x else m) 0 ns,0)
    empty ns = snd $ head $ Map.toList $ Map.filter ((==) 0 . _nodeUsed) ns
    fromSuccess (Success x) = x
    fromSuccess (Failure x) = error (show x)
    windowsNewLine = void $ skipOptional newline <* skipOptional (char '\r')

nodeParser :: Parser Node
nodeParser = Node <$>
            coordParser <*>
            numParser 'T' <*>
            numParser 'T' <*
            numParser 'T' <*
            numParser '%'
  where
    coordParser = string "/dev/grid/node-x" *> ((,) <$> integer <*> (string "-y" *> integer))
    numParser c = whiteSpace *> integer <* (char c <* whiteSpace)

isViable :: Node -> Node -> Bool
isViable (Node c1 _ u1) (Node c2 s2 u2)
  | c1 == c2  = False
  | otherwise = u1 > 0 && u1 <= a2
  where a2 = s2 - u2

distance :: NodeCoord -> NodeCoord -> Integer
distance (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)

moveData :: (Node,Node) -> (Node,Node)
moveData (Node c1 s1 u1, Node c2 s2 u2) = (Node c1 s1 0, Node c2 s2 (u1+u2))

moves :: NodeState -> [NodeState]
moves (NodeState goal empty ns steps) = newStates
  where
    newStates = map (newState . moveData) (filter (uncurry isViable) nodesToMove)
    newState c = NodeState (newGoal c) (newEmpty c) (adjustedNodes c) (steps+1)
    newGoal (a,b)
      | _nodeCoord a == _nodeCoord goal = b
      | otherwise = goal
    newEmpty (b,a)
      | _nodeCoord a == _nodeCoord empty = b
      | otherwise = empty
    adjustMap a = Map.adjust (const a) (_nodeCoord a)
    adjustedNodes (a,b) = adjustMap b $ adjustMap a ns
    distanceFromEtoG = distance (_nodeCoord empty) (_nodeCoord goal)
    nodesToMove
      |  distanceFromEtoG == 1 = goalAdjacent ++ emptyAdjacent
      | otherwise              = emptyAdjacent
    adjacents (x,y) = mapMaybe (`Map.lookup` ns) [(x-1,y),(x,y-1),(x,y+1),(x+1,y)]
    emptyAdjacent = map (\n -> (n,empty)) $ adjacents (_nodeCoord empty)
    goalAdjacent = map (\n -> (goal,n)) $ adjacents (_nodeCoord goal)

isGoal :: NodeState -> Bool
isGoal (NodeState goal _ _ _) = _nodeCoord goal == (0,0)

heuristic :: NodeState -> Integer
heuristic (NodeState goal empty _ _) = (5 * distanceToGoal) + distanceToEmpty
  where
    goalNodeCoord = _nodeCoord goal
    distanceToGoal = distance (0,0) goalNodeCoord
    distanceToEmpty = distance goalNodeCoord (_nodeCoord empty)

viables :: [Node] -> [(Node,Node)]
viables ns = [(a,b) | a <- ns, b <- ns, isViable a b]

part1 :: String -> Int
part1 = length . viables . Map.elems . _nodeStateNodes . parseInput

printMoves :: NodeState -> IO (HashSet NodeState)
printMoves s = do
  let mvs = moves s
      steps = _stepsTaken <$> headMay mvs
  print steps
  visualize s
  return $ HS.fromList mvs

part2 :: String -> Maybe Int
part2 s = length <$> aStar (HS.fromList . moves) (const $ const 1) heuristic isGoal (parseInput s)

part2Print :: String -> IO (Maybe Int)
part2Print s = fmap length <$> aStarM printMoves (\_ _ -> return 1) (return . heuristic) (return . isGoal) (return $ parseInput s)

profilePart2 :: (NodeState -> HashSet NodeState) -> (NodeState -> Integer) -> String -> Maybe Int
profilePart2 m h s = length <$> aStar m (const $ const 1) h isGoal (parseInput s)

part2Solution :: IO (Maybe Int)
part2Solution = part2 <$> readFile "./data/Day22.txt"

visualize :: NodeState -> IO ()
visualize (NodeState g e ns _) = mapM_ putStrLn l
  where
    ordered = map (sortOn (fst .fst)) $
      groupBy (\((_,y),_) ((_,y'),_) -> y==y') $
      sortOn (snd . fst) $
      Map.toList ns
    l = map (intersperse ' ' . map (c . snd)) ordered
    c n
      | e == n         = '_'
      | g == n         = 'G'
      | isViable n e   = '.'
      | otherwise      = '#'

test1 :: String
test1 = "/dev/grid/node-x0-y0   10T    8T     2T   80%\n/dev/grid/node-x0-y1   11T    6T     5T   54%\n/dev/grid/node-x0-y2   32T   28T     4T   87%\n/dev/grid/node-x1-y0    9T    7T     2T   77%\n/dev/grid/node-x1-y1    8T    0T     8T    0%\n/dev/grid/node-x1-y2   11T    7T     4T   63%\n/dev/grid/node-x2-y0   10T    6T     4T   60%\n/dev/grid/node-x2-y1    9T    8T     1T   88%\n/dev/grid/node-x2-y2    9T    6T     3T   66%"
test2 :: String
test2 = "/dev/grid/node-x0-y0   10T    8T     2T   80%\n/dev/grid/node-x0-y1   11T    6T     5T   54%\n/dev/grid/node-x0-y2   32T   28T     4T   87%\n/dev/grid/node-x0-y3   32T   28T     4T   87%\n/dev/grid/node-x1-y0    9T    7T     2T   77%\n/dev/grid/node-x1-y1    8T    0T     8T    0%\n/dev/grid/node-x1-y2   11T    7T     4T   63%\n/dev/grid/node-x1-y3   11T    7T     4T   63%\n/dev/grid/node-x2-y0   10T    6T     4T   60%\n/dev/grid/node-x2-y1    9T    8T     1T   88%\n/dev/grid/node-x2-y2    9T    6T     3T   66%\n/dev/grid/node-x2-y3    9T    6T     3T   66%\n/dev/grid/node-x3-y0   10T    6T     4T   60%\n/dev/grid/node-x3-y1    9T    8T     1T   88%\n/dev/grid/node-x3-y2    9T    6T     3T   66%\n/dev/grid/node-x3-y3    9T    6T     3T   66%"
test3 :: String
test3 = "/dev/grid/node-x0-y0   10T    8T     2T   80%\n/dev/grid/node-x0-y1   11T    6T     5T   54%\n/dev/grid/node-x0-y2   32T   28T     4T   87%\n/dev/grid/node-x0-y3   32T   28T     4T   87%\n/dev/grid/node-x0-y4   32T   28T     4T   87%\n/dev/grid/node-x1-y0    9T    7T     2T   77%\n/dev/grid/node-x1-y1    8T    0T     8T    0%\n/dev/grid/node-x1-y2   11T    7T     4T   63%\n/dev/grid/node-x1-y3   11T    7T     4T   63%\n/dev/grid/node-x1-y4   11T    7T     4T   63%\n/dev/grid/node-x2-y0   10T    6T     4T   60%\n/dev/grid/node-x2-y1    9T    8T     1T   88%\n/dev/grid/node-x2-y2    9T    6T     3T   66%\n/dev/grid/node-x2-y3    9T    6T     3T   66%\n/dev/grid/node-x2-y4    9T    6T     3T   66%\n/dev/grid/node-x3-y0   10T    6T     4T   60%\n/dev/grid/node-x3-y1    9T    8T     1T   88%\n/dev/grid/node-x3-y2    9T    6T     3T   66%\n/dev/grid/node-x3-y3    9T    6T     3T   66%\n/dev/grid/node-x3-y4    9T    6T     3T   66%\n/dev/grid/node-x4-y0   10T    6T     4T   60%\n/dev/grid/node-x4-y1    9T    8T     1T   88%\n/dev/grid/node-x4-y2    9T    6T     3T   66%\n/dev/grid/node-x4-y3    9T    6T     3T   66%\n/dev/grid/node-x4-y4    9T    6T     3T   66%"
