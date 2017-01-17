module Main where

import           Control.Monad
import           Criterion.Main
import           Day23

main :: IO ()
main =
  part2Solution >>= print
  -- part2Print test1 >>= print
  -- do
  --   f <- readFile "./data/Day22.txt"
  --   _ <- part2Print f
  --   return ()
  -- defaultMain
  --   [ bgroup "moves"
  --       [ bench "test2" $ whnf (profilePart2 moves heuristic') test2
  --       ]
  --   , bgroup "moves'"
  --       [ bench "test2" $ whnf (profilePart2 moves' heuristic') test2
  --       ]
  --   ]
