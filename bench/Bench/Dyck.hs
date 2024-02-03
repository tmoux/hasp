module Bench.Dyck where

import Criterion.Main
import Data.Bifunctor (second)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Hasp.Examples.Dyck as H
import qualified Dyck as P
import qualified Data.Text.IO as Text
import Data.Text (unpack)

parseFromFile :: IO String -> (String -> Maybe Int) -> Benchmarkable
parseFromFile input = whnfAppIO (<$> input)

files :: M.Map String (IO String)
files =
  M.fromList $
   map (second (fmap unpack . Text.readFile))
    [ ("5000", "bench/data/dyck/dyck.5000"),
      ("10000", "bench/data/dyck/dyck.10000"),
      ("15000", "bench/data/dyck/dyck.15000"),
      ("20000", "bench/data/dyck/dyck.20000"),
      ("25000", "bench/data/dyck/dyck.25000"),
      ("30000", "bench/data/dyck/dyck.30000")
    ]

benchSize :: (String -> Maybe Int) -> String -> Benchmark
benchSize f size = bench size $ parseFromFile (fromJust (M.lookup size files)) f

benchSizes :: (String -> Maybe Int) -> [String] -> [Benchmark]
benchSizes f = map (benchSize f)

dyckBench :: Benchmark
dyckBench =
  bgroup
    "dyck"
    [ bgroup
        "hasp"
        (benchSizes H.countParens sizes),
      bgroup
        "parsec"
        (benchSizes P.countParens sizes)
    ]
  where
    sizes =
      [ "5000",
        "10000",
        "15000",
        "20000",
        "25000",
        "30000"
      ]
