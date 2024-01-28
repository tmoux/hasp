{-# LANGUAGE DataKinds #-}

module Main (main) where

import Control.Applicative
import Control.Monad.Except (runExcept)
import Data.Either (isLeft)
import Data.Maybe
import Data.Set (fromList)
import Hasp.Combinators
import Hasp.Hoas
import Hasp.Parser
import Hasp.Typecheck
import Hasp.Types
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Ingredients.Rerun
import Prelude hiding (null)

-- Define parsers
-- Typecheck parsers

data Sexp = Sym Char | SSeq [Sexp]
  deriving (Show, Eq)

letter :: Hoas Char
letter = charset ['a' .. 'z']

word :: Hoas [Char]
word = star letter

-- Parse a language of s-expressions
sexp :: Hoas Sexp
sexp = fix $
  \p -> Sym <$> letter <|> SSeq <$> paren (star p)

getRight :: Either a b -> b
getRight (Left _) = error "got left"
getRight (Right x) = x

makeParser :: Hoas a -> Parser a
-- makeParser = toParser . fromJust . typecheck . toTerm
makeParser = toParser . getRight . runExcept . typecheck . toTerm

main :: IO ()
main = defaultMainWithRerun tests

parseEps :: Parser ()
parseEps = makeParser (eps ())

parseParens :: Parser Char
parseParens = makeParser (paren (chr 'c'))

parseWord :: Parser [Char]
parseWord = makeParser word

parseAlt :: Parser Char
parseAlt = makeParser (chr 'c' <|> chr 'd')

parseStarParen :: Parser [Char]
parseStarParen = makeParser $ star (paren $ chr 'c')

parseSexp :: Parser Sexp
parseSexp = makeParser sexp

parseSexpChar :: Parser (Sexp, Char)
parseSexpChar = makeParser $ (,) <$> sexp <*> letter

parseMultiSexp :: Parser [Sexp]
parseMultiSexp = makeParser $ star sexp

sexpType :: TCMonad Tp
sexpType = snd <$> typecheck (toTerm sexp)

multiSexpType :: TCMonad Tp
multiSexpType = snd <$> typecheck (toTerm (star sexp))

badFixpoint :: TCMonad Tp
badFixpoint = snd <$> typecheck (toTerm (fix (\p -> eps 'c' <|> p *> chr 'c')))

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ testGroup
        "Unit tests"
        [ testCase "hoas eps" $ unP parseEps "abc" @?= Just ((), "abc"),
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
