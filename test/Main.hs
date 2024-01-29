module Main (main) where

import qualified ParsingTests
import Test.Tasty
import Test.Tasty.Ingredients.Rerun
import qualified TypingTests
import Prelude hiding (null)

main :: IO ()
main = defaultMainWithRerun tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ ParsingTests.tests,
      TypingTests.tests
    ]
