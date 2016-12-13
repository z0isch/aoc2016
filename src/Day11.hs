module Day11 (part1, part1', part2, test1) where

import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set

data Gen = Gen String
  deriving (Eq,Ord,Show)
data Chip = Chip String
  deriving (Eq,Ord,Show)

type ElevatorFloor = Int
data ProbState = ProbState ElevatorFloor (Map Int (Set Gen)) (Map Int (Set Chip))
  deriving (Eq,Ord,Show)

validFloor :: Set Gen -> Set Chip -> Bool
validFloor gs cs
  | Set.null gs = True
  | otherwise = all (`Set.member` gs) $ Set.map (\(Chip c) -> Gen c) cs

validState :: ProbState -> Bool
validState (ProbState _ gs' cs') = and $ zipWith validFloor (Map.elems gs') (Map.elems cs')

--nextStates :: ProbState -> [ProbState]
nextStates (ProbState e gs cs) = filter validState
                                $ nub
                                $ concat
                                $ mixedStates ++ singleGStates ++ doubleGStates ++ singleCStates ++ doubleCStates
  where
    singleGStates = map (\nE -> map (f nE . (:[])) singleGs) nextElevators
    doubleGStates = map (\nE -> map (f nE) doubleGs) nextElevators
    singleCStates = map (\nE ->  map (h nE . (:[])) singleCs) nextElevators
    doubleCStates = map (\nE ->  map (h nE) doubleCs) nextElevators
    mixedStates = map (\nE -> map (\(g,c) -> ProbState nE (Map.adjust (Set.insert g) nE (Map.adjust (Set.delete g) e gs)) (Map.adjust (Set.insert c) nE (Map.adjust (Set.delete c) e cs))) mixed) nextElevators
    f nE gs' = ProbState nE (foldl' (\m g -> Map.adjust (Set.insert g) nE m) (foldl' (\m g -> Map.adjust (Set.delete g) e m) gs gs') gs') cs
    h nE cs' = ProbState nE gs (foldl' (\m c -> Map.adjust (Set.insert c) nE m) (foldl' (\m c -> Map.adjust (Set.delete c) e m) cs cs') cs')
    nextElevators = [nE | nE <- [e-1,e+1], nE >= 0 && nE < 4]
    singleGs = Set.toList (Map.findWithDefault Set.empty e gs)
    doubleGs = [[g1,g2] | g1 <- singleGs, g2 <- singleGs, g1 /= g2]
    singleCs = Set.toList (Map.findWithDefault Set.empty e cs)
    doubleCs = [[c1,c2] | c1 <- singleCs,c2 <- singleCs, c1 /= c2]
    mixed = [(g,c) | g@(Gen gStr) <-singleGs, c@(Chip cStr) <- singleCs, gStr == cStr]

part1 = find (== test1) $ head $ dropWhile (notElem test1) $ iterate (\ss -> foldl' (flip delete) (concatMap nextStates ss) ss) [startState]
part1' = length $ iterate (\ss -> foldl' (flip delete) (concatMap nextStates ss) ss) [startState] !!  9
part2 = undefined

startState = ProbState 3
              (Map.fromList [(0,Set.empty), (1,Set.empty), (2,Set.empty), (3,Set.fromList [Gen"H",Gen"L"])])
              (Map.fromList [(0,Set.empty), (1,Set.empty), (2,Set.empty), (3,Set.fromList [Chip "H",Chip "L"])])

test1 = ProbState 0
          (Map.fromList [(0,Set.empty), (1,Set.fromList [Gen "H"]),(2,Set.fromList [Gen"G"]),(3,Set.empty)])
          (Map.fromList [(0,Set.fromList [Chip "H",Chip "L"]),(1,Set.empty),(2,Set.empty),(3,Set.empty)])
