{-# LANGUAGE DataKinds #-}

module DgnfTests where

import Control.Monad.Except (runExcept)
import Control.Monad.ST (runST)
import Data.GADT.Compare (GCompare, GEq)
import Data.GADT.Show (GShow)
import Data.Some (Some)
import Hasp.Char (char)
import Hasp.ClosedDgnf (convertToClosed, resolve)
import Hasp.Combinators (between, choice, many)
import Hasp.Grammar (Grammar)
import Hasp.Hoas (Hoas, fix, toTerm, tok)
import Hasp.Normalization (normalize)
import Hasp.Parser (Parser, parse)
import Hasp.Resolved (parserFromNT)
import Hasp.Stream (Stream, Tag (..))
import Hasp.Typecheck (typecheck)
import Hasp.Types (Tp)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Control.Applicative ((<|>))

convertDGNF :: (Stream s t, GShow t, GCompare t) => Grammar '[] t a (Tp (Some t)) -> Parser s a
convertDGNF g = runST $ parserFromNT . resolve . convertToClosed <$> normalize g

makeParserDGNF :: (Stream s t, GShow t, GCompare t) => Hoas t a -> Parser s a
makeParserDGNF p = case parser of
  Left err -> error err
  Right result -> result
  where
    parser = runExcept $ do
      typechecked <- typecheck (toTerm p)
      return (convertDGNF typechecked)

checkParser ::
  (Stream s t, GEq t, Show s, Eq s, Show a, Eq a, GShow t, GCompare t) =>
  Hoas t a -> -- parser
  s -> -- input
  a -> -- expected output
  s -> -- expect rest of the stream
  Assertion
checkParser parser input output rest =
  let p = makeParserDGNF parser
   in parse p input @?= Just (output, rest)

-- p ::= (p*) | z
-- semantic action: count number of z's
sexpParser :: Hoas (Tag Char) Int
sexpParser = fix $ \p ->
  choice
    [ between (tok (Tag '(')) (tok (Tag ')')) (sum <$> many p),
      1 <$ tok (Tag 'z')
    ]

-- p ::= a(a*)b
-- semantic action: count number of a's
abParser :: Hoas (Tag Char) Int
abParser = fix $ \p ->
  choice
    [ (+ 1) <$ char 'a' <*> p,
      0 <$ char 'b'
    ]

altParser :: Hoas (Tag Char) Int
altParser = sexpParser <|> abParser

-- Test direct conversion from DGNF to
test_naiveDgnfParsing :: TestTree
test_naiveDgnfParsing =
  testGroup
    "Naive DGNF Parsing"
    [ testCase "sexp 1" (checkParser sexpParser "((z)()((z)))" 2 ""),
      testCase "sexp 2" (checkParser sexpParser "((z)())xcd" 1 "xcd"),
      testCase "sexp 3" (checkParser sexpParser "((z)(((z)))zzz)" 5 ""),
      testCase "ab 1" (checkParser abParser "aaab" 3 ""),
      testCase "ab 2" (checkParser abParser "ab" 1 ""),
      testCase "alt 1" (checkParser altParser "((z)()((z)))" 2 ""),
      testCase "alt 2" (checkParser altParser "aaaab" 4 "")
    ]
