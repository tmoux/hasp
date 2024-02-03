module Main where

import Bench.Sexp
import Bench.Dyck
import Criterion.Main

-- The function we're benchmarking.
fib :: Int -> Int
fib m
  | m < 0 = error "negative!"
  | otherwise = go m
  where
    go 0 = 0
    go 1 = 1
    go n = go (n - 1) + go (n - 2)

fib' :: Int -> Int
fib' x = fs !! x
  where
    fs = 0 : 1 : zipWith (+) fs (tail fs)

main :: IO ()
main =
  defaultMain
    [ -- bgroup
      --   "fib"
      --   [ bench "1" $ whnf fib 1,
      --     bench "5" $ whnf fib 5,
      --     bench "9" $ whnf fib 9,
      --     bench "11" $ whnf fib 11
      --   ],
      -- bgroup
      --   "fib'"
      --   [ bench "1" $ whnf fib' 1,
      --     bench "5" $ whnf fib' 5,
      --     bench "9" $ whnf fib' 9,
      --     bench "11" $ whnf fib' 11
      --   ],
      dyckBench,
      sexpBench
    ]
