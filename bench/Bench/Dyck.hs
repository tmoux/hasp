module Bench.Dyck where

import Criterion.Main
import Data.Bifunctor (second)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text.IO as Text
import qualified Dyck as P
import qualified Hasp.Examples.Dyck as H

parseFrom :: IO a -> (a -> Maybe Int) -> Benchmarkable
parseFrom input = whnfAppIO (<$> input)

files :: M.Map String (IO Text)
files =
  M.fromList $
    map
      (second Text.readFile)
      [ ("5000", "bench/data/dyck/dyck.5000")
      , ("10000", "bench/data/dyck/dyck.10000")
      , ("15000", "bench/data/dyck/dyck.15000")
      , ("20000", "bench/data/dyck/dyck.20000")
      , ("25000", "bench/data/dyck/dyck.25000")
      , ("30000", "bench/data/dyck/dyck.30000")
      ]

benchSize :: (Text -> Maybe Int) -> String -> Benchmark
benchSize f size = bench size $ parseFrom (fromJust (M.lookup size files)) f

benchSizes :: (Text -> Maybe Int) -> [String] -> [Benchmark]
benchSizes f = map (benchSize f)

dyckBench :: Benchmark
dyckBench =
  bgroup
    "dyck"
    [ bgroup
        "hasp text"
        (benchSizes H.countParens sizes)
    , bgroup
        "parsec text"
        (benchSizes P.countParens sizes)
    , bgroup
        "handwritten parser"
        (benchSizes P.handWritten sizes)
    , bgroup
        "handwritten char tag"
        (benchSizes P.handWrittenCharTag sizes)
    ]
 where
  sizes =
    [ "5000"
    , "10000"
    , "15000"
    , "20000"
    , "25000"
    , "30000"
    ]
