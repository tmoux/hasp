{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

module TypingTests where

import Control.Monad.Except (runExcept)
import Data.Either (isLeft)
-- import Data.Set (fromList)
import Data.Some
import Hasp.Hoas
import Hasp.Typecheck
import Hasp.Types
import Parsers
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (null)
import Data.GADT.Show
import Data.GADT.Compare

-- TODO: why do we need these other constraints?
checkIllTyped :: (GShow t, GCompare t) => Hoas t a -> Assertion
checkIllTyped parser =
  assertBool "should not typecheck" (isLeft p)
  where
    p = runExcept (snd <$> typecheck (toTerm parser))

checkType :: (Show (Some t), Ord (Some t)) => Hoas t a -> Tp (Some t) -> Assertion
checkType parser tp =
  case runExcept (snd <$> typecheck (toTerm parser)) of
    Left err -> error err
    Right t -> t @?= tp

tests :: TestTree
tests =
  testGroup
    "Typechecking tests"
    [ testCase "bad fixpoint" $ checkIllTyped hBadFixpoint,
      testCase "bad disjunction" $ checkIllTyped hBadDisj
      {-
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
            -}
    ]
