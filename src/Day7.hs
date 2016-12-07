module Day7 (part1, part2, test1, test2) where

import           Control.Applicative
import           Data.List
import           Data.List.Split
import           Text.Trifecta

data IPv7Part = Hypernet String | Supernet String
  deriving (Show, Eq, Ord)

type IPv7 = [IPv7Part]

hypernetParser :: Parser IPv7Part
hypernetParser = Hypernet <$> brackets (some letter)

ipv7parser :: Parser IPv7
ipv7parser = some (try hypernetParser <|> (Supernet <$> some letter))

parseInput :: String -> Result [IPv7]
parseInput = parseString (some (ipv7parser <* skipOptional newline)) mempty

getSectionLengths :: Int -> String -> [String]
getSectionLengths i = filter ((== i) . length) . concatMap (chunksOf i) . tails

abaSections :: String -> [String]
abaSections = filter isAba . getSectionLengths 3
  where
    isAba [x,y,z] = x == z && y /= x
    isAba _       = False

abbaSections :: String -> [String]
abbaSections = filter isAbba . getSectionLengths 4
  where
    isAbba [p,q,r,s] = p == s && q == r && p /= q
    isAbba _         = False

supportsTLS :: IPv7 -> Bool
supportsTLS ip = any supernetAbba ip && not (any hypernetAbba ip)
  where
    supernetAbba (Supernet x) = not $ null $ abbaSections x
    supernetAbba _            = False
    hypernetAbba (Hypernet x) = not $ null $ abbaSections x
    hypernetAbba _            = False

sslParts :: IPv7 -> [(String,String)]
sslParts ip = [(x,y) | x <- abas, y <- babs, babEqAba x y]
  where
    abas = concatMap supernetAba ip
    babs = concatMap hypernetBab ip
    supernetAba (Supernet x) = abaSections x
    supernetAba _            = []
    hypernetBab (Hypernet x) = abaSections x
    hypernetBab _            = []
    babEqAba [x,y,_] [x',y',_'] = x == y' && y == x'
    babEqAba _ _                = False

supportsSSL :: IPv7 -> Bool
supportsSSL = not . null . sslParts

fromSuccess :: Result x -> x
fromSuccess (Success x) = x
fromSuccess (Failure x) = error (show x)

part1 :: String -> [Bool]
part1= map supportsTLS . fromSuccess . parseInput

part2 :: String -> [Bool]
part2=  map supportsSSL . fromSuccess . parseInput

part1Solution :: IO Integer
part1Solution = genericLength . filter (True ==) . part1 <$> readFile "./data/Day7.txt"

part2Solution :: IO Integer
part2Solution = genericLength . filter (True ==) . part2 <$> readFile "./data/Day7.txt"

test1 :: String
test1 = "abba[mnop]qrst\nabcd[bddb]xyyx\naaaa[qwer]tyui\nioxxoj[asdfgh]zxcvbn"

test2 :: String
test2 = "aba[bab]xyz\nxyx[xyx]xyx\naaa[kek]eke\nzazbz[bzb]cdb"
