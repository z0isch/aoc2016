{-# LANGUAGE DeriveGeneric #-}
module Day24 (part1, part2, test1, part1Solution,part2Solution, part1SolutionM) where

import           Data.Graph.AStar
import           Data.Hashable
import qualified Data.HashSet     as HS
import           Data.List
import           Data.Map.Strict  (Map)
import qualified Data.Map.Strict  as M
import           Data.Maybe
import qualified Data.Set         as S
import           GHC.Generics     (Generic)

data Tile = Wall | Open | Wire Int
  deriving (Eq,Ord,Show, Generic)
instance Hashable Tile

type Coord = (Int,Int)
type Duct = Map Coord Tile
type NumWires = Int
type WiresVisited = [Int]

data RobotState = RobotState
  { _robotStateNumWires     :: NumWires
  , _robotStateStartCoord   :: Coord
  , _robotStateCoord        :: Coord
  , _robotStateWiresVisited :: WiresVisited
  , _robotStateDuct         :: Duct
  }
  deriving (Eq, Show, Ord, Generic)
instance Hashable RobotState where
  hashWithSalt salt (RobotState nw sc c ws duct) = mapHashed
    where
      mapHashed = M.foldl' step (salt `hashWithSalt` nw) duct `hashWithSalt` c `hashWithSalt` sc `hashWithSalt` ws
      step n a  = n `hashWithSalt` a

data Direction = U | D | L | R
  deriving (Eq,Show,Enum)

startState :: String -> RobotState
startState s =  RobotState numWires startPos startPos [] duct
  where
    numWires = M.size $ M.filter isWireTile duct
    isWireTile (Wire _) = True
    isWireTile _ = False
    duct = fmap (fromMaybe Open) mbDuct
    startPos = head $ M.keys $ M.filter isNothing mbDuct
    mbDuct = foldl' mapInsert M.empty $ zip [0..] $ map (map ductLine) $ lines s
    mapInsert m (y,ts) = foldl' (\m' (x,t) -> M.insert (x,y) t m') m $ zip [0..] ts
    ductLine '#' = Just Wall
    ductLine '.' = Just Open
    ductLine '0' = Nothing
    ductLine x   = Just (Wire (read [x]))

moves :: RobotState -> [RobotState]
moves r@(RobotState _ _ c _ duct) = map (move r) dirs
  where
    dirs = map fst $ filter (isValid . snd) $ zip [U ..] $ map (getCoord c) [U ..]
    isValid c' = M.member c' duct && tileValid (duct M.! c')
    tileValid Wall = False
    tileValid _ = True

move :: RobotState -> Direction -> RobotState
move (RobotState nW sc c wires  duct) d = RobotState nW sc nextCoord (newWires nextTile) (newDuct nextTile)
  where
    newDuct (Wire _) = M.adjust (const Open) nextCoord duct
    newDuct _ = duct
    newWires (Wire i) = i:wires
    newWires _ = wires
    nextTile = duct M.! nextCoord
    nextCoord = getCoord c d

getCoord :: Coord -> Direction -> Coord
getCoord (x,y) U = (x,y-1)
getCoord (x,y) D = (x,y+1)
getCoord (x,y) L = (x-1,y)
getCoord (x,y) R = (x+1,y)

foundAllWires :: RobotState -> Bool
foundAllWires (RobotState nw _ _ ws _) = length ws == nw

backAtStart :: RobotState -> Bool
backAtStart (RobotState _ sc c _ _) = sc == c

dist :: Coord -> Coord -> Int
dist (x,y) (x',y') = abs(x-x') + abs(y-y')

heurstic :: RobotState -> Int
heurstic rs@(RobotState nw _ c ws _) = minDist
  where
    (_,_,minDist) = head $
                    dropWhile (\(_,wSet,_) -> S.size wSet < nw) $
                    iterate (goToClosest rs) (c,S.fromList ws,0)

heurstic2 :: RobotState -> Int
heurstic2 rs@(RobotState _ sc c _ _)
  | foundAllWires rs = dist sc c
  | otherwise = heurstic rs

goToClosest :: RobotState -> (Coord, S.Set Int, Int) -> (Coord, S.Set Int, Int)
goToClosest rs@(RobotState _ _ _ _ duct) (c',wSet,d) = (c'',S.insert wire wSet ,d+d')
  where
    (c'', d') = closestWire rs c' wSet
    wire = let (Wire i) = duct M.! c'' in i

closestWire :: RobotState -> Coord -> S.Set Int -> (Coord, Int)
closestWire (RobotState _ _ _ _ duct) c' wSet = head $
                                              sortOn snd $
                                              M.toList $
                                              M.mapMaybeWithKey wireDist duct
  where
    wireDist c'' (Wire i)
      | i `S.member` wSet = Nothing
      | otherwise = Just $ dist c' c''
    wireDist _ _ = Nothing


part1 :: String -> Maybe Int
part1 s = fmap length $ aStar (HS.fromList . moves) (const . const 1) heurstic foundAllWires $ startState s
part1Solution :: IO (Maybe Int)
part1Solution = part1 <$> readFile "./data/Day24.txt"

heursticM :: RobotState -> IO Int
heursticM a = do
  let h = heurstic a
  print h
  return h
part1M :: String -> IO (Maybe Int)
part1M s = fmap (fmap length) $ aStarM (return . HS.fromList . moves) (const . const (return 1)) heursticM (return . foundAllWires) $ return $ startState s
part1SolutionM :: IO (Maybe Int)
part1SolutionM = do
  d <- readFile "./data/Day24.txt"
  part1M d

part2 :: String -> Maybe Int
part2 = fmap length . aStar (HS.fromList . moves) (const . const 1) heurstic2 (\r -> foundAllWires r && backAtStart r) . startState
part2Solution :: IO (Maybe Int)
part2Solution = part2 <$> readFile "./data/Day24.txt"

test1 :: String
test1 = "###########\n#0.1.....2#\n#.#######.#\n#4.......3#\n###########"
