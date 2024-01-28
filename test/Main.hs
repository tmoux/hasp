{-# LANGUAGE DataKinds #-}

module Main (main) where

import Control.Applicative
import Control.Monad.Except (runExcept)
import Data.Maybe
import Data.Set (fromList)
import qualified Hasp.Hoas as H
import Hasp.Parser
import Hasp.Types
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Ingredients.Rerun
import Prelude hiding (null)
import Data.Either (isLeft)

data Sexp = Sym Char | SSeq [Sexp]
  deriving (Show, Eq)

letter :: H.Hoas Char
letter = H.charset ['a' .. 'z']

word :: H.Hoas [Char]
word = H.star letter

-- Parse a language of s-expressions
sexp :: H.Hoas Sexp
sexp = H.fix $
  \p -> Sym <$> letter <|> SSeq <$> H.paren (H.star p)

getRight :: Either a b -> b
getRight (Left _) = error "got left"
getRight (Right x) = x

makeParser :: H.Hoas a -> Parser a
-- makeParser = toParser . fromJust . typecheck . H.toTerm
makeParser = toParser . getRight . runExcept . typecheck . H.toTerm

main :: IO ()
main = defaultMainWithRerun tests

parseA :: Grammar ctx Char d
parseA = (Chr 'a', undefined)

parseB :: Grammar ctx Char d
parseB = (Chr 'b', undefined)

parseEps :: Parser ()
parseEps = makeParser (H.eps ())

parseParens :: Parser Char
parseParens = makeParser (H.paren (H.chr 'c'))

parseWord :: Parser [Char]
parseWord = makeParser word

parseAlt :: Parser Char
parseAlt = makeParser (H.chr 'c' <|> H.chr 'd')

parseStarParen :: Parser [Char]
parseStarParen = makeParser $ H.star (H.paren $ H.chr 'c')

parseSexp :: Parser Sexp
parseSexp = makeParser sexp

parseSexpChar :: Parser (Sexp, Char)
parseSexpChar = makeParser $ H.seq sexp letter

parseMultiSexp :: Parser [Sexp]
parseMultiSexp = makeParser $ H.star sexp

sexpType :: TCMonad Tp
sexpType = snd <$> typecheck (H.toTerm sexp)

multiSexpType :: TCMonad Tp
multiSexpType = snd <$> typecheck (H.toTerm (H.star sexp))

badFixpoint :: TCMonad Tp
badFixpoint = snd <$> typecheck (H.toTerm (H.fix (\p -> H.eps 'c' <|> p *> H.chr 'c')))

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
          testCase "hoas sexp" $ unP parseSexp "(abc)" @?= Just (SSeq [Sym 'a', Sym 'b', Sym 'c'], ""),
          testCase "hoas sexp 2" $ unP parseSexp "(a(b(f)c))" @?= Just (SSeq [Sym 'a', SSeq [Sym 'b', SSeq [Sym 'f'], Sym 'c']], ""),
          testCase "hoas sexp char" $ unP parseSexpChar "(abc)deef" @?= Just ((SSeq [Sym 'a', Sym 'b', Sym 'c'], 'd'), "eef"),
          testCase "hoas multi" $
            unP parseMultiSexp "(abc)()(c)d"
              @?= Just
                ( [ SSeq [Sym 'a', Sym 'b', Sym 'c'],
                    SSeq [],
                    SSeq [Sym 'c'],
                    Sym 'd'
                  ],
                  ""
                )
        ],
      testGroup
        "type checking tests"
        [ testCase "bad fixpoint" $ assertBool "Does not typecheck" (isLeft $ runExcept badFixpoint),
          testCase "sexp" $
            runExcept sexpType
              @?= Right
                ( Tp
                    { null = False,
                      first = fromList "(abcdefghijklmnopqrstuvwxyz",
                      flast = fromList "",
                      guarded = True
                    }
                ),
          testCase "multisexp" $
            runExcept multiSexpType
              @?= Right
                ( Tp
                    { null = True,
                      first = fromList "(abcdefghijklmnopqrstuvwxyz",
                      flast = fromList "(abcdefghijklmnopqrstuvwxyz",
                      guarded = True
                    }
                )
        ]
    ]
