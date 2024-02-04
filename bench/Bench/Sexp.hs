module Bench.Sexp where

import Criterion.Main
import Data.Bifunctor (second)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text.IO as Text
import qualified Hasp.Examples.Sexp as H
import qualified Sexp as P

parseFromFile :: IO a -> (a -> Maybe Int) -> Benchmarkable
parseFromFile input = whnfAppIO (<$> input)

files :: M.Map String (IO Text)
files =
  M.fromList $
    map
      (second Text.readFile)
      [ ("50", "bench/data/sexp/sexp.50"),
        ("1024", "bench/data/sexp/sexp.1024"),
        ("2048", "bench/data/sexp/sexp.2048"),
        ("2048", "bench/data/sexp/sexp.2048"),
        ("3072", "bench/data/sexp/sexp.3072"),
        ("4096", "bench/data/sexp/sexp.4096"),
        ("262144", "bench/data/sexp/sexp.262144"),
        ("524288", "bench/data/sexp/sexp.524288"),
        ("786432", "bench/data/sexp/sexp.786432")
      ]

benchSize :: (Text -> Maybe Int) -> String -> Benchmark
benchSize f size = bench size $ parseFromFile (fromJust (M.lookup size files)) f

benchSizes :: (Text -> Maybe Int) -> [String] -> [Benchmark]
benchSizes f = map (benchSize f)

sexpBench :: Benchmark
sexpBench =
  bgroup
    "sexp"
    [ bgroup
        "hasp"
        (benchSizes H.countAtoms sizes),
      bgroup
        "parsec"
        (benchSizes P.countAtoms sizes)
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
