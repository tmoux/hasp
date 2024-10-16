{-# LANGUAGE TemplateHaskell #-}

module Hasp.Examples.Sexp where

-- Parse s-expressions with alphanumeric atoms

import Data.GADT.Compare.TH
import Data.GADT.Show.TH
import Data.Some
import Hasp.Char
import Hasp.Combinators
import Hasp.Examples.LexStream
import Hasp.Examples.Parsers (makeParser)
import Hasp.Hoas
import Hasp.Parser (Parser (..), parse)
import Hasp.Stream

-- | Let's parse a language of s-expressions, where atoms are alphanumeric strings.
-- Our parser will return the number of atoms in the s-expression.
-- We will split up the parsing into a lexing and parsing phase.
-- That is, instead of parsing a string directly, we will first write a parser that will parse the first token from the string, and use this to construct a new Stream type that outputs tokens.
-- (Alternatively, we could parse all the tokens first, put it in a list, and then parse that list; I'm not sure which is better performance-wise.)
-- Our language of s-expressions is E ::= Atom | ( E* )
-- We thus have three different types of tokens, left parentheses, right parentheses, and atoms.
-- Each token has an associated data type. "Keywords"/punctuation such as parentheses carry a unit, and Atoms carry a string. An Integer token would have an Int type, and so on.

-- | We represent these using a GADT, where each token tag is represented by a distinct data constructor:
data TTag a where
  LP :: TTag ()
  RP :: TTag ()
  Atom :: TTag String

-- | We use some Template Haskell to automatically derive the equivalent of Show and Ord instances for GADTs.
-- Writing these instances by hand is straightforward but tedious.
deriveGShow ''TTag
deriveGEq ''TTag
deriveGCompare ''TTag

-- TEST: parser without lexing stage
-- TODO: how to write valid grammar including whitespace?
-- sexpChar :: Hoas CharTag Int
-- sexpChar = fix $ \p ->
--   choice
--     [ 1 <$ alpha <* many alphaNum,
--       between (char '(') (char ')') (sum <$> (p `sepBy` some space))
--     ]
--
-- countAtoms' :: String -> Maybe Int
-- countAtoms' s = fst <$> parse (makeParser sexpChar) s

-- Our parser will process a stream consisting of tokens indexed by CharTag (a tag for each possible Char) and will return Some (Token TTag), or a (Token TTag a) for some type a.
-- It ignores whitespace and tokenizes alphanumeric strings and left/right parentheses.
token :: Hoas CharTag (Some (Token TTag))
token = fix $ \p ->
  choice
    [ parseTok,
      space *> p
    ]
  where
    parseTok :: Hoas CharTag (Some (Token TTag))
    parseTok =
      choice
        [ mkSome (Token LP ()) <$ char '(',
          mkSome (Token RP ()) <$ char ')',
          mkSome . Token Atom <$> ((:) <$> alpha <*> many alphaNum)
        ]

-- Construct a parser from the HOAS DSL
lexerP :: (Stream s CharTag) => Parser s (Some (Token TTag))
lexerP = makeParser token

parseTokens :: (Stream s CharTag) => s -> SomeToks TTag
parseTokens st = SomeToks (go st)
  where
    go s = case parse lexerP s of
      Nothing -> []
      Just (t, rest) -> t : go rest

-- This is our actual parser: it takes a stream of TTags and returns an Int (the number of atoms)
parseSexp :: Hoas TTag Int
parseSexp = fix $ \p ->
  choice
    [ between (tok LP) (tok RP) (sum <$> many p),
      1 <$ tok Atom
    ]

-- A helper function to parse a string and return the number of atoms (or Nothing if parsing fails)
countAtoms :: (Stream s CharTag) => s -> Maybe Int
countAtoms s = fst <$> parse (makeParser parseSexp) (makeLexStream lexerP s)

-- Helper function that does all lexing first
countAtoms' :: (Stream s CharTag) => s -> Maybe Int
countAtoms' s = let tokens = parseTokens s in fst <$> parse (makeParser parseSexp) tokens
