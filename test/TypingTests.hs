module TypingTests where

import Control.Monad.Except (runExcept)
import Data.Either (isLeft)
import Data.Set (fromList)
import Hasp.Hoas
import Hasp.Typecheck
import Hasp.Types
import Parsers
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (null)

checkIllTyped :: Hoas a -> Assertion
checkIllTyped parser =
  assertBool "should not typecheck" (isLeft p)
  where
    p = runExcept (snd <$> typecheck (toTerm parser))

checkType :: Hoas a -> Tp -> Assertion
checkType parser tp =
  case runExcept (snd <$> typecheck (toTerm parser)) of
    Left err -> error err
    Right t -> t @?= tp

tests :: TestTree
tests =
  testGroup
    "Typechecking tests"
    [ testCase "fixpoint" $ checkIllTyped hBadFixpoint,
      testCase "disjunction" $ checkIllTyped hBadDisj,
      testCase "sexp" $
        checkType
          sexp
          Tp
            { null = False,
              first = fromList "(abcdefghijklmnopqrstuvwxyz",
              flast = fromList "",
              guarded = True
            },
      testCase "multisexp" $
        checkType
          hMultiSexp
          Tp
            { null = True,
              first = fromList "(abcdefghijklmnopqrstuvwxyz",
              flast = fromList "(abcdefghijklmnopqrstuvwxyz",
              guarded = True
            }
    ]
