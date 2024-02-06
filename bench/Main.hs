module Main where

import Bench.Dyck
import Bench.Sexp
import Criterion.Main

main :: IO ()
main =
  defaultMain
    [ dyckBench
    , sexpBench
    ]
