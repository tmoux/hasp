{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Sexp where

-- Parse s-expressions with alphanumeric atoms

import Control.Monad.Except (runExcept)
import Data.GADT.Compare
import Data.GADT.Compare.TH
  ( DeriveGCompare (deriveGCompare),
    DeriveGEQ (deriveGEq),
  )
import Data.GADT.Show
import Data.GADT.Show.TH
import Data.Some
import Hasp.Char
import Hasp.Combinators
import Hasp.Hoas
import Hasp.Parser (Parser (..), parse, toParser)
import Hasp.Stream
import Hasp.Typecheck

-- | Let's parse a language of s-expressions, where atoms are alphanumeric strings.
-- Our parser will return the number of atoms in the s-expression.
-- We will split up the parsing into a lexing and parsing phase.
-- That is, instead of parsing a string directly, we will first write a parser that will parse the first token from the string, and use this to construct a new Stream type that outputs tokens.
-- (Alternatively, we could parse all the tokens first, put it in a list, and then parse that list, I'm not sure which is better performance-wise.)
-- Our language of s-expressions is E ::= Atom | ( E* )
-- We thus have three different types of tokens, left parentheses, right parentheses, and atoms.
-- Each token has an associated data type. "Keywords"/punctuation such as parentheses carry a unit, and Atoms carry a string. An Integer token would have an Int type, and so on.

-- | We represent these using a GADT, where each token tag is represented by a distinct data constructor:
-- data TTag a where
--   LP :: TTag ()
--   RP :: TTag ()
--   Atom :: TTag String
-- 
-- -- | We use some Template Haskell to automatically derive the equivalent of Show and Ord instances for GADTs.
-- -- Writing these instances by hand is straightforward but tedious.
-- deriveGShow ''TTag
-- deriveGEq ''TTag
-- deriveGCompare ''TTag
-- 
-- -- Our parser will process a stream consisting of tokens indexed by CharTag (a tag for each possible Char) and will return Some (Token TTag), or a (Token TTag a) for some type a.
-- -- It ignores whitespace and tokenizes alphanumeric strings and left/right parentheses.
-- token :: Hoas CharTag (Some (Token TTag))
-- token = fix $ \p ->
--   choice
--     [ parseTok,
--       space *> p
--     ]
--   where
--     parseTok :: Hoas CharTag (Some (Token TTag))
--     parseTok =
--       choice
--         [ mkSome (Token LP ()) <$ char '(',
--           mkSome (Token RP ()) <$ char ')',
--           mkSome . Token Atom <$> ((:) <$> alpha <*> many alphaNum)
--         ]
-- 
-- -- TODO: move this to common library and use compile-time check
-- makeParser :: (Stream s t, GCompare t, GShow t) => Hoas t a -> TCMonad (Parser s a)
-- makeParser p = toParser <$> typecheck (toTerm p)
-- 
-- makeParse :: (Stream s t, GCompare t, GShow t) => Hoas t a -> Parser s a
-- makeParse p = case runExcept (makeParser p) of
--   Left err -> error err
--   Right p' -> p'
-- 
-- lexerP :: Parser String (Some (Token TTag))
-- lexerP = makeParse token
-- 
-- -- A datatype representing streams s tokenizing to t
-- data LexStream s t = Lex (s -> Maybe (Some t, s)) s
-- 
-- makeStream :: String -> LexStream String (Token TTag)
-- makeStream = Lex (parse lexerP)
-- 
-- instance Stream (LexStream s (Token t)) t where
--   uncons (Lex f s) =
--     f s >>= \(t, s') -> Just (t, Lex f s')
-- 
-- countParen :: Hoas TTag [()]
-- countParen = many (tok LP)
-- 
-- countParenP :: (Stream s TTag) => Parser s [()]
-- countParenP = makeParse countParen
-- 
-- -- parseSexp is a parser that takes a stream of TTags and returns an Int (the number of atoms)
-- parseSexp :: Hoas TTag Int
-- parseSexp = fix $ \p ->
--   choice
--     [ between (tok LP) (tok RP) (sum <$> many p),
--       1 <$ tok Atom
--     ]
-- 
-- s1 :: LexStream String (Token TTag)
-- s1 = makeStream "(a b c (d e f))"
-- 
-- countAtoms :: String -> Maybe Int
-- countAtoms s = fst <$> parse (makeParse parseSexp) (makeStream s)
