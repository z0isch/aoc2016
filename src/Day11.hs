{-# LANGUAGE DeriveGeneric #-}
module Day11 (part1, part2, test1, testStartState, part1Solution,part2Solution) where

import           Control.Arrow    (second)
import           Data.Foldable
import           Data.Graph.AStar
import           Data.Hashable
import qualified Data.HashSet     as H
import           Data.List
import           Data.Map.Strict  (Map)
import qualified Data.Map.Strict  as Map
import           Data.Set         (Set)
import qualified Data.Set         as Set
import           GHC.Generics     (Generic)

data Gen = Gen String
  deriving (Eq,Ord,Show,Generic)
instance Hashable Gen
data Chip = Chip String
  deriving (Eq,Ord,Show,Generic)
instance Hashable Chip

type ElevatorFloor = Int
data ProbState = ProbState ElevatorFloor (Map Int (Set Gen)) (Map Int (Set Chip))
  deriving (Eq,Ord,Show)
data HProbState = HProbState ElevatorFloor [(Int,[Gen])] [(Int,[Chip])]
  deriving (Eq,Ord,Show,Generic)
instance Hashable HProbState

fromHState :: HProbState -> ProbState
fromHState (HProbState e gs cs) = ProbState e (Map.fromList $ map (second Set.fromList) gs) (Map.fromList $ map (second Set.fromList) cs)
toHState :: ProbState -> HProbState
toHState (ProbState e gs cs) = HProbState e (map (second Set.toList) $ Map.toList gs) (map (second Set.toList) $ Map.toList cs)

validFloor :: Set Gen -> Set Chip -> Bool
validFloor gs cs
  | Set.null gs = True
  | otherwise = all (`Set.member` gs) $ Set.map (\(Chip c) -> Gen c) cs

validState :: ProbState -> Bool
validState (ProbState _ gs' cs') = and $ zipWith validFloor (Map.elems gs') (Map.elems cs')

nextStates :: ProbState -> [ProbState]
nextStates (ProbState e gs cs) = filter validState
                                $ concat . concat
                                $ [mixedStates,singleGStates,doubleGStates,singleCStates,doubleCStates]
  where
    singleGStates = map (\nE -> map (f nE . (:[])) singleGs) nextElevators
    doubleGStates = map (\nE -> map (f nE) doubleGs) notDownNextElevators
    singleCStates = map (\nE ->  map (h nE . (:[])) singleCs) nextElevators
    doubleCStates = map (\nE ->  map (h nE) doubleCs) notDownNextElevators
    mixedStates = map (\nE -> map (\(g,c) -> ProbState nE (Map.adjust (Set.insert g) nE (Map.adjust (Set.delete g) e gs)) (Map.adjust (Set.insert c) nE (Map.adjust (Set.delete c) e cs))) mixed) notDownNextElevators
    f nE gs' = ProbState nE (foldl' (\m g -> Map.adjust (Set.insert g) nE m) (foldl' (\m g -> Map.adjust (Set.delete g) e m) gs gs') gs') cs
    h nE cs' = ProbState nE gs (foldl' (\m c -> Map.adjust (Set.insert c) nE m) (foldl' (\m c -> Map.adjust (Set.delete c) e m) cs cs') cs')
    nextElevators = [nE | nE <- [e-1,e+1], nE >= 0 && nE < 4]
    notDownNextElevators = [nE | nE <- [e+1], nE >= 0 && nE < 4]
    singleGs = Set.toList (Map.findWithDefault Set.empty e gs)
    doubleGs = [[g1,g2] | g1 <- singleGs, g2 <- singleGs, g1 /= g2]
    singleCs = Set.toList (Map.findWithDefault Set.empty e cs)
    doubleCs = [[c1,c2] | c1 <- singleCs,c2 <- singleCs, c1 /= c2]
    mixed = [(g,c) | g@(Gen gStr) <-singleGs, c@(Chip cStr) <- singleCs, gStr == cStr]

heuristic :: HProbState -> Int
heuristic (HProbState _ gs cs) = movesToTop cs + movesToTop gs
  where
    movesToTop = sum . map (\(f,c) -> (3-f) * length c)

part1 s e = length <$> aStar (H.fromList . map toHState . nextStates . fromHState) (const . const 1) heuristic (toHState s ==) (toHState e)
part2 = part1

part1Solution = part1 inputStartState input
part2Solution = part2 input2StartState input2

probStateViz :: ProbState -> IO ()
probStateViz (ProbState e gs cs) = mapM_ putStrLn floors
  where
    floors = map showFloor $ reverse $ sortOn (\(f,_,_) -> f) $ Map.elems $ Map.mapWithKey (\f g -> (f,g,Map.findWithDefault Set.empty f cs)) gs
    showFloor (f,gfs,cfs) = show f ++ " " ++ (if f == e then "E" else " ") ++ " " ++ foldMap shortG gfs ++ foldMap shortC cfs
    shortG (Gen g) = g++"G "
    shortC (Chip c) = c++"M "

input2 =  ProbState 0
          (Map.fromList [(0,Set.fromList [Gen "T", Gen "Pl",Gen "S",Gen "E",Gen "D"]), (1,Set.empty),(2,Set.fromList [Gen"Pr", Gen"R"]),(3,Set.empty)])
          (Map.fromList [(0,Set.fromList [Chip "T",Chip "E",Chip "D"]),(1,Set.fromList [Chip "Pl",Chip "S"]),(2,Set.fromList [Chip "Pr",Chip "R"]),(3,Set.empty)])
input2StartState = ProbState 3
              (Map.fromList [(0,Set.empty), (1,Set.empty), (2,Set.empty), (3,Set.fromList [Gen"T",Gen"Pl",Gen"S",Gen"Pr",Gen"R",Gen"E",Gen"D"])])
              (Map.fromList [(0,Set.empty), (1,Set.empty), (2,Set.empty), (3,Set.fromList [Chip"T",Chip"Pl",Chip"S",Chip"Pr",Chip"R",Chip"E",Chip"D"])])
inputStartState = ProbState 3
              (Map.fromList [(0,Set.empty), (1,Set.empty), (2,Set.empty), (3,Set.fromList [Gen"T",Gen"Pl",Gen"S",Gen"Pr",Gen"R"])])
              (Map.fromList [(0,Set.empty), (1,Set.empty), (2,Set.empty), (3,Set.fromList [Chip"T",Chip"Pl",Chip"S",Chip"Pr",Chip"R"])])
input = ProbState 0
          (Map.fromList [(0,Set.fromList [Gen "T", Gen "Pl",Gen "S"]), (1,Set.empty),(2,Set.fromList [Gen"Pr", Gen"R"]),(3,Set.empty)])
          (Map.fromList [(0,Set.fromList [Chip "T"]),(1,Set.fromList [Chip "Pl",Chip "S"]),(2,Set.fromList [Chip "Pr",Chip "R"]),(3,Set.empty)])

testStartState = ProbState 3
              (Map.fromList [(0,Set.empty), (1,Set.empty), (2,Set.empty), (3,Set.fromList [Gen"H",Gen"L"])])
              (Map.fromList [(0,Set.empty), (1,Set.empty), (2,Set.empty), (3,Set.fromList [Chip "H",Chip "L"])])
test2 = ProbState 0
          (Map.fromList [(0,Set.fromList [Gen "T", Gen "Pl",Gen "S"]), (1,Set.empty),(2,Set.fromList [Gen"Pr", Gen"R"]),(3,Set.empty)])
          (Map.fromList [(0,Set.fromList [Chip "T",Chip "Pl"]),(1,Set.fromList [Chip "S"]),(2,Set.fromList [Chip "Pr",Chip "R"]),(3,Set.empty)])
test1 = ProbState 0
          (Map.fromList [(0,Set.empty), (1,Set.fromList [Gen "H"]),(2,Set.fromList [Gen"L"]),(3,Set.empty)])
          (Map.fromList [(0,Set.fromList [Chip "H",Chip "L"]),(1,Set.empty),(2,Set.empty),(3,Set.empty)])
