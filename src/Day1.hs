module Day1 where

import           Control.Applicative
import           Control.Monad       (join)
import           Data.Bifunctor      (bimap)
import           Data.List
import qualified Data.Map.Strict     as M
import           Text.Trifecta

data Orientation = N | S | E | W
  deriving (Show,Eq)

data TurnDir = L | R
  deriving (Show,Eq)

data Action = Turn TurnDir | Move
  deriving (Show,Eq)

turnParser :: Parser TurnDir
turnParser = try (L <$ char 'L') <|> (R <$ char 'R')

walkParser :: Parser [(TurnDir,Integer)]
walkParser = commaSep1 ((,) <$> turnParser <*> integer)

makeAction :: (TurnDir,Integer) -> [Action]
makeAction (L,i) = Turn L: genericTake (i-1) (repeat Move)
makeAction (R,i) = Turn R: genericTake (i-1) (repeat Move)

step :: (Orientation,(Integer,Integer)) -> Action -> (Orientation,(Integer,Integer))
step (N,(x,y)) (Turn L) = (W,(x,y))
step (N,(x,y)) (Turn R) = (E,(x,y))
step (N,(x,y)) Move     = (N,(x,y+1))
step (S,(x,y)) (Turn L) = (E,(x,y))
step (S,(x,y)) (Turn R) = (W,(x,y))
step (S,(x,y)) Move     = (S,(x,y-1))
step (E,(x,y)) (Turn L) = (N,(x,y))
step (E,(x,y)) (Turn R) = (S,(x,y))
step (E,(x,y)) Move     = (E,(x+1,y))
step (W,(x,y)) (Turn L) = (S,(x,y))
step (W,(x,y)) (Turn R) = (N,(x,y))
step (W,(x,y)) Move     = (W,(x-1,y))

doWalk :: [Action] -> [(Orientation,(Integer,Integer))]
doWalk = scanl' step (N,(0,0))

walk :: Result [(Orientation, (Integer, Integer))]
walk = doWalk . concatMap makeAction <$> parseString walkParser mempty input

squashTurns :: [(Orientation,(Integer,Integer))] -> [(Orientation,(Integer,Integer))]
squashTurns = foldl' f []
  where
    f [] p = [p]
    f ps p
      | snd (last ps) == snd p = ps
      | otherwise              = ps ++ [p]

makeRevisitMap :: [(Orientation,(Integer,Integer))] -> M.Map (Integer,Integer) Integer
makeRevisitMap = M.filter (==2) . foldl' (\m (_,p) -> M.insertWith (+) p 1 m) M.empty . squashTurns

part1 :: IO ()
part1 = case walk of
  Failure d -> print d
  Success w -> print $ uncurry (+) $ join bimap abs $ snd $ last w

part2 :: IO ()
part2 = case walk of
  Failure d -> print d
  Success w -> print $ uncurry (+) $ join bimap abs $ fst $ head sortedByFirstRevist
    where
      sortedByFirstRevist = sortOn (last . snd) $ M.toList indexMap
      indexMap = M.mapWithKey (\p _ -> elemIndices p $ map snd w) $ makeRevisitMap w

input :: String
input ="R4, R3, R5, L3, L5, R2, L2, R5, L2, R5, R5, R5, R1, R3, L2, L2, L1, R5, L3, R1, L2, R1, L3, L5, L1, R3, L4, R2, R4, L3, L1, R4, L4, R3, L5, L3, R188, R4, L1, R48, L5, R4, R71, R3, L2, R188, L3, R2, L3, R3, L5, L1, R1, L2, L4, L2, R5, L3, R3, R3, R4, L3, L4, R5, L4, L4, R3, R4, L4, R1, L3, L1, L1, R4, R1, L4, R1, L1, L3, R2, L2, R2, L1, R5, R3, R4, L5, R2, R5, L5, R1, R2, L1, L3, R3, R1, R3, L4, R4, L4, L1, R1, L2, L2, L4, R1, L3, R4, L2, R3, L1, L5, R4, R5, R2, R5, R1, R5, R1, R3, L3, L2, L2, L5, R2, L2, R5, R5, L2, R3, L5, R5, L2, R4, R2, L1, R3, L5, R3, R2, R5, L1, R3, L2, R2, R1"
