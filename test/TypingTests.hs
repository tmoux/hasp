{-# LANGUAGE FlexibleContexts #-}

module TypingTests where

import Control.Monad.Except (runExcept)
import Data.Either (isLeft)
import Data.GADT.Compare
import Data.GADT.Show
import Data.Set (fromList)
import Data.Some
import Hasp.Examples.Parsers
import Hasp.Hoas
import Hasp.Stream (Tag (Tag))
import Hasp.Typecheck
import Hasp.Types
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (null)

checkIllTyped :: (GShow t, GCompare t) => Hoas t a -> Assertion
checkIllTyped parser =
  assertBool "should not typecheck" (isLeft p)
 where
  p = runExcept (snd <$> typecheck (toTerm parser))

checkType :: (GShow t, GCompare t) => Hoas t a -> Tp (Some t) -> Assertion
checkType parser tp =
  case runExcept (snd <$> typecheck (toTerm parser)) of
    Left err -> error err
    Right t -> t @?= tp

test_typechecking :: TestTree
test_typechecking =
  testGroup
    "Typechecking tests"
    [ testCase "bad fixpoint" $ checkIllTyped hBadFixpoint
    , testCase "bad disjunction" $ checkIllTyped hBadDisj
    , testCase "sexp" $
        checkType
          sexp
          Tp
            { null = False
            , first = fromList $ makeSomeTag <$> "(abcdefghijklmnopqrstuvwxyz"
            , flast = fromList $ makeSomeTag <$> ""
            , guarded = True
            }
    , testCase
        "multisexp"
        $ checkType
          hMultiSexp
          Tp
            { null = True
            , first = fromList $ makeSomeTag <$> "(abcdefghijklmnopqrstuvwxyz"
            , flast = fromList $ makeSomeTag <$> "(abcdefghijklmnopqrstuvwxyz"
            , guarded = True
            }
    ]
 where
  makeSomeTag :: a -> Some (Tag a)
  makeSomeTag c = mkSome (Tag c)
