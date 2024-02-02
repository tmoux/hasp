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
import Data.GADT.Show
import Data.GADT.Show.TH
import Data.Some
import Hasp.Char
import Hasp.Combinators
import Hasp.Hoas
import Hasp.Parser (Parser (..), parse, toParser)
import Hasp.Stream
import Hasp.Typecheck

data TTag a where
  LP :: TTag ()
  RP :: TTag ()
  Atom :: TTag String

deriveGShow ''TTag
deriveGEq ''TTag
deriveGCompare ''TTag

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

-- TODO: move this to common library and use compile-time check
makeParser :: (Stream s t, GCompare t, GShow t) => Hoas t a -> TCMonad (Parser s a)
makeParser p = toParser <$> typecheck (toTerm p)

makeParse :: (Stream s t, GCompare t, GShow t) => Hoas t a -> Parser s a
makeParse p = case runExcept (makeParser p) of
  Left err -> error err
  Right p' -> p'

lexerP :: Parser String (Some (Token TTag))
lexerP = makeParse token

-- A datatype representing streams s tokenizing to t
data LexStream s t = Lex (s -> Maybe (Some t, s)) s

makeStream :: String -> LexStream String (Token TTag)
makeStream = Lex (parse lexerP)

instance Stream (LexStream s (Token t)) t where
  uncons (Lex f s) =
    f s >>= \(t, s') -> Just (t, Lex f s')

countParen :: Hoas TTag [()]
countParen = many (tok LP)

countParenP :: (Stream s TTag) => Parser s [()]
countParenP = makeParse countParen

parseSexp :: Hoas TTag Int
parseSexp = fix $ \p ->
  choice
    [ between (tok LP) (tok RP) (sum <$> many p),
      1 <$ tok Atom
    ]

s1 :: LexStream String (Token TTag)
s1 = makeStream "(a b c (d e f))"

countAtoms :: String -> Maybe Int
countAtoms s = fst <$> parse (makeParse parseSexp) (makeStream s)
