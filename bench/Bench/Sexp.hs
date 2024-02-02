module Bench.Sexp where

import Criterion.Main
import Data.Bifunctor (second)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Hasp.Examples.Sexp as H
import qualified Sexp as P

parseFromFile :: IO String -> (String -> Maybe Int) -> Benchmarkable
parseFromFile input = whnfAppIO (<$> input)

files :: M.Map String (IO String)
files =
  M.fromList $
    second readFile
      <$> [ ("50", "bench/data/sexp/sexp.50"),
            ("1024", "bench/data/sexp/sexp.1024"),
            ("2048", "bench/data/sexp/sexp.2048"),
            ("2048", "bench/data/sexp/sexp.2048"),
            ("3072", "bench/data/sexp/sexp.3072"),
            ("4096", "bench/data/sexp/sexp.4096"),
            ("262144", "bench/data/sexp/sexp.262144"),
            ("524288", "bench/data/sexp/sexp.524288"),
            ("786432", "bench/data/sexp/sexp.786432")
          ]

benchSize :: (String -> Maybe Int) -> String -> Benchmark
benchSize f size = bench size $ parseFromFile (fromJust $ M.lookup size files) f

benchSizes :: (String -> Maybe Int) -> [String] -> [Benchmark]
benchSizes f = map (benchSize f)

sexpBench :: Benchmark
sexpBench =
  bgroup
    "sexp"
    [ bgroup
        "parsec"
        (benchSizes P.countAtoms sizes),
      bgroup
        "hasp"
        (benchSizes H.countAtoms sizes)
    ]
  where
    sizes =
      [ "1024",
        "2048",
        "3072",
        "4096"
      ]

-- [ "262144",
--   "524288",
--   "786432"
-- ]
