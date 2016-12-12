module Day10 (part1, part2, test1, ChipHolder(..)) where

import           Control.Applicative
import           Data.List
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map
import           Data.Set            (Set)
import qualified Data.Set            as Set
import           Text.Trifecta

data ChipHolder = Bot Integer | Output Integer
  deriving (Eq, Show, Ord)

data Instruction = TakeVal (Integer, ChipHolder) | GiveVal ChipHolder (ChipHolder, ChipHolder)
  deriving (Eq, Show)

data FactoryState = FactoryState
  { _chipState    :: Map ChipHolder [Integer]
  , _compareState :: Map ChipHolder (Set Integer)
  }
  deriving (Eq, Show)

startState :: FactoryState
startState = FactoryState Map.empty Map.empty

instructionParser :: Parser Instruction
instructionParser = try takeParser <|> giveParser
  where
    botParser = string "bot " *> (Bot <$> natural)
    outputParser = string "output " *>  (Output <$> natural)
    chipHolderParser = try botParser <|> outputParser
    takeParser = TakeVal <$> (string "value " *> ((,) <$> natural <*> (string "goes to " *> chipHolderParser)))
    giveParser = (\c1 _ c2 _ c3 -> GiveVal c1 (c2,c3)) <$> botParser <*> string "gives low to " <*> chipHolderParser <*> string "and high to " <*> chipHolderParser

parseInput :: String -> Result [Instruction]
parseInput = parseString (some (instructionParser <* skipOptional windowsNewLine)) mempty
  where windowsNewLine = const () <$ skipOptional newline <*> skipOptional (char '\r')

fromSuccess :: Result x -> x
fromSuccess (Success x) = x
fromSuccess (Failure x) = error (show x)

giveChip :: Map ChipHolder [Integer] -> Integer -> ChipHolder -> Map ChipHolder [Integer]
giveChip chipState i c = Map.insertWithKey f c [i] chipState
  where
    f (Output _) _ chipList = i:chipList
    f _ _ chipList
      | length chipList == 2 = chipList
      | otherwise             = sort (i:chipList)

doInstruction :: FactoryState -> Instruction -> FactoryState
doInstruction fs (GiveVal c1 (c2,c3))
  | length chipList == 2 = FactoryState cs compS
  | otherwise           = fs
  where
    chipList = Map.findWithDefault [] c1 (_chipState fs)
    [low,high] = chipList
    cs = Map.delete c1 $ giveChip (giveChip (_chipState fs) low c2) high c3
    compS = Map.insertWith Set.union c1 (Set.fromList chipList) (_compareState fs)
doInstruction fs (TakeVal (i,c)) = FactoryState (giveChip (_chipState fs) i c) (_compareState fs)

part1 :: Integer -> Integer -> String -> ChipHolder
part1 i1 i2 = fst . head . Map.toList . compared . head . dropWhile (not . seenNeeded) . scanl' doInstruction startState . cycle . fromSuccess . parseInput
  where
    compared = Map.filter f . _compareState
    f s = Set.member i1 s && Set.member i2 s
    seenNeeded = not . null . compared

part2 :: String -> Map ChipHolder [Integer]
part2 = compared . head . dropWhile (not . seenNeeded) . scanl' doInstruction startState . cycle . fromSuccess . parseInput
  where
    compared = Map.filterWithKey f . _chipState
    f (Output i) xs
      | i == 0 || i == 1 || i == 2 = not $ null xs
      | otherwise = False
    f _ _            = False
    seenNeeded = (==) 3 . length . compared

test1 :: String
test1="value 5 goes to bot 2\nbot 2 gives low to bot 1 and high to bot 0\nvalue 3 goes to bot 1\nbot 1 gives low to output 1 and high to bot 0\nbot 0 gives low to output 2 and high to output 0\nvalue 2 goes to bot 2"
