module Day5 (part1, part2, test1) where

import           Data.Hash.MD5
import           Data.List
import           Data.Maybe
import qualified Data.Sequence as S
import           Safe

isValidHash :: String -> Bool
isValidHash s = all (== '0') (take 5 s)

isValidHash2 :: String -> Bool
isValidHash2 s = isValidHash s && isValidPosition (readMay [s !! 5])
  where
    isValidPosition :: Maybe Integer -> Bool
    isValidPosition Nothing = False
    isValidPosition (Just p)
      | p < 8 && p >= 0 = True
      | otherwise = False

part1 :: String -> String
part1 c =
    take 8 $
    map (!! 5) $
    filter isValidHash $
    map (md5s . Str . (++) c . show) ([0 ..] :: [Integer])

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) =x : if p x
        then takeWhileInclusive p xs
        else []

part2 :: String -> String
part2 c =
    foldMap ((: []) . fromJust) $
    last $
    takeWhileInclusive ((7 >=) . S.length . S.takeWhileL isJust) $
    scanl' setFold (S.fromList $ replicate 8 Nothing) $
    map getCoords $
    filter isValidHash2 $
    map (md5s . Str . (++) c . show) ([0 ..] :: [Integer])
  where
    getCoords :: String -> (Int, Char)
    getCoords x = (read [x !! 5] :: Int, x !! 6)
    setFold :: S.Seq (Maybe Char) -> (Int, Char) -> S.Seq (Maybe Char)
    setFold xs (p,x) =
        case xs `S.index` p of
            Nothing -> S.update p (Just x) xs
            _       -> xs
test1 :: String
test1 = "abc"
input1 :: String
input1 = "ojvtpuvg"
