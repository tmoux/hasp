{-# LANGUAGE AllowAmbiguousTypes #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module ParsingTests where

import Control.Monad.Except (runExcept)
import Hasp.Combinators
import Hasp.Hoas
import Hasp.Parser
import Hasp.Typecheck
import Parsers
import Test.Tasty
import Test.Tasty.HUnit

makeParser :: (Stream s t, Show t, Ord t) => Hoas t a -> TCMonad (Parser s a)
makeParser p = toParser <$> typecheck (toTerm p)

checkParser :: (Stream s t, Show s, Eq s, Show a, Eq a, Show t, Ord t) => Hoas t a -> s -> a -> s -> Assertion
checkParser parser input output rest =
  case runExcept (makeParser parser) of
    Left err -> error err
    Right p -> parse p input @?= Just (output, rest)

tests :: TestTree
tests =
  testGroup
    "Parsing tests"
    [ testCase "eps" $ checkParser hEps "abc" () "abc",
      testCase "parens" $ checkParser hParens "(c)" 'c' "",
      testCase "word" $ checkParser word "abcd abc" "abcd" " abc",
      testCase "alt" $ checkParser hAlt "cdc abc" 'c' "dc abc",
      testCase "star parens" $ checkParser hStarParen "(c)(c)(c)[]" "ccc" "[]",
      testCase "sexp" $ checkParser sexp "(abc)" (SSeq [Sym 'a', Sym 'b', Sym 'c']) "",
      testCase "sexp 2" $ checkParser sexp "(a(b(f)c))" (SSeq [Sym 'a', SSeq [Sym 'b', SSeq [Sym 'f'], Sym 'c']]) "",
      testCase "sexp char" $ checkParser hSexpChar "(abc)deef" (SSeq [Sym 'a', Sym 'b', Sym 'c'], 'd') "eef",
      testCase "multi sexp" $
        checkParser
          hMultiSexp
          "(abc)()(c)d"
          [ SSeq [Sym 'a', Sym 'b', Sym 'c'],
            SSeq [],
            SSeq [Sym 'c'],
            Sym 'd'
          ]
          "",
      testCase "dyck 1" $ checkParser hDyck "()((()))(()())" 7 "",
      testCase "dyck 2" $ checkParser hDyck "(((())))()()()()" 8 "",
      testCase "dyck 3" $ checkParser hDyck "(())()()" 4 "",
      testCase "option 1" $ checkParser (option $ char 'c') "c" (Just 'c') "",
      testCase "option 2" $ checkParser (option $ letter <* char 'c') "ac" (Just 'a') "",
      testCase "option 3" $ checkParser (option $ letter <* char 'c') "3c" Nothing "3c",
      testCase "adds chainr" $ checkParser hAddsR "3+5+9" ((C 3) :+: ((C 5) :+: (C 9))) "",
      testCase "adds chainl" $ checkParser hAddsL "3+5+9" (((C 3) :+: (C 5)) :+: (C 9)) ""
    ]
