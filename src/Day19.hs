module Day19 (part1, part2, test1, input1) where

import qualified Data.Sequence as S

data ElfState = ElfState Int (S.Seq Int)
  deriving (Eq, Show)

start :: Int -> ElfState
start i = ElfState 0 $ S.fromList [1..i]

move :: (S.Seq Int -> Int -> Int) -> ElfState -> ElfState
move f (ElfState s v) = ElfState ns nextV
  where
    removeI = f v s
    nextV = remove removeI v
    nextS = if removeI < s then s else s+1
    ns = nextS `mod` fromIntegral (S.length nextV)

leftOf :: S.Seq Int -> Int -> Int
leftOf v s = (s + 1) `mod` fromIntegral (S.length v)

accrossFrom :: S.Seq Int -> Int -> Int
accrossFrom v s = (s + n) `mod` l
  where
    l = S.length v
    n = l `div` 2

remove :: Int -> S.Seq Int -> S.Seq Int
remove i v = f $ S.splitAt i v
    where
      f (a,b)
        | S.null a = S.drop 1 b
        | otherwise = a S.>< S.drop 1 b


part1, part2 :: Int -> Int
part1 i = (\(ElfState _ v) -> v `S.index` 0 ) $
            head $
            dropWhile (\(ElfState _ v) -> S.length v > 1) $
            iterate (move leftOf) (start i)
part2 i = (\(ElfState _ v) -> v `S.index` 0) $
            head $
            dropWhile (\(ElfState _ v) -> S.length v > 1) $
            iterate (move accrossFrom) (start i)

test1,input1 :: Int
test1 = 5
input1 = 3005290
