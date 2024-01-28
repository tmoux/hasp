{-# LANGUAGE DataKinds #-}

module Main (main) where

import Control.Applicative
import qualified Hasp.Hoas as H
import Hasp.Parser
import Hasp.Types
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Ingredients.Rerun

main :: IO ()
main = defaultMainWithRerun tests

parseA :: Grammar ctx Char d
parseA = (Chr 'a', undefined)

parseB :: Grammar ctx Char d
parseB = (Chr 'b', undefined)

parseEps :: Parser ()
parseEps = H.makeParser (H.eps ())

parseParens :: Parser Char
parseParens = H.makeParser (H.paren (H.chr 'c'))

parseWord :: Parser [Char]
parseWord = H.makeParser H.word

parseAlt :: Parser Char
parseAlt = H.makeParser (H.chr 'c' <|> H.chr 'd')

parseStarParen :: Parser [Char]
parseStarParen = H.makeParser $ H.star (H.paren $ H.chr 'c')

-- sexpCheck :: TCMonad (Grammar '[] H.Sexp Tp)
-- sexpCheck = typecheck (H.toTerm H.sexp)

parseSexp :: Parser H.Sexp
parseSexp = H.makeParser H.sexp

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ testGroup
        "Unit tests"
        [ testCase "parse Char" $ unP (parse parseA HNil) "abc" @?= Just ('a', "bc"),
          testCase "parse Char" $ unP (parse (Seq parseA parseB, undefined) HNil) "abc" @?= Just (('a', 'b'), "c"),
          testCase "parse Char into int" $
            unP (parse (Map (const 7) parseA, undefined) HNil) "abc"
              @?= Just
                (7 :: Int, "bc"),
          testCase "hoas eps" $ unP parseEps "abc" @?= Just ((), "abc"),
          testCase "hoas parens" $ unP parseParens "(c)" @?= Just ('c', ""),
          testCase "hoas word" $ unP parseWord "abcd abc" @?= Just ("abcd", " abc"),
          testCase "hoas alt" $ unP parseAlt "cdc" @?= Just ('c', "dc"),
          testCase "hoas star parens" $ unP parseStarParen "(c)(c)(c)[]" @?= Just ("ccc", "[]"),
          testCase "hoas sexp" $ unP parseSexp "(abc)" @?= Just (H.SSeq [H.Sym 'a', H.Sym 'b', H.Sym 'c'], "")
        ],
      testGroup
        "type checking tests"
        []
    ]
