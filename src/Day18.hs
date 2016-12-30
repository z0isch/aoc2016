module Day18 (part1,part2, genNext, test1, input1) where

import           Data.List
import           Data.List.Split

genNext :: String -> String
genNext i = map genChar $ [['.',head i,i!!1]] ++ middle ++ [[i!!(length i -2),last i,'.']]
  where
    middle :: [String]
    middle =  filter ((==) 3 . length) $ map (take 3) $ tails i
    genChar ['^','^','.'] = '^'
    genChar ['.','^','^'] = '^'
    genChar ['^','.','.'] = '^'
    genChar ['.','.','^'] = '^'
    genChar _             = '.'

part1 i r = length $ concatMap (filter ('.' ==)) $ take r $ iterate genNext i
part2 = part1

test1,input1 :: String
test1=".^^.^.^^^^"
input1="......^.^^.....^^^^^^^^^...^.^..^^.^^^..^.^..^.^^^.^^^^..^^.^.^.....^^^^^..^..^^^..^^.^.^..^^..^^^.."
