{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Sexp where

-- Parse s-expressions with alphanumeric atoms

import Control.Monad.Except (runExcept)
import Data.Functor.Identity
import Hasp.Char
import Hasp.Combinators
import Hasp.Hoas
import Hasp.Parser (Parser (..), Stream, parse, toParser)
import Hasp.Typecheck
import Hasp.Types (Tp (..))
import qualified Text.Parsec as Parsec

data Token = LP | RP | Atom String
  deriving (Show, Eq, Ord)

token :: Hoas Char Token
token = fix $ \p ->
  choice
    [ parseTok,
      space *> p
    ]
  where
    parseTok :: Hoas Char Token
    parseTok =
      (LP <$ char '(')
        <|> (RP <$ char ')')
        <|> Atom
        <$> ((:) <$> alpha <*> many alphaNum)

--- makeParser :: (Stream s t, Show t, Ord t) => Hoas t a -> (Parser s a)
makeParser p = toParser <$> typecheck (toTerm p)

makeParse p = case runExcept (makeParser p) of
  Left err -> error err
  Right p' -> p'

ltype p = case runExcept (typecheck (toTerm p)) of
  Left err -> error err
  Right t -> snd t

lexerType :: Tp Char
lexerType = ltype token

lexerP :: Parser String Token
lexerP = makeParse token

-- A datatype representing streams s tokenizing to t
data LexStream s t = Lex (s -> Maybe (t, s)) s

makeStream :: String -> LexStream String Token
makeStream = Lex (parse lexerP)

instance Parsec.Stream (LexStream s t) Identity t where
  uncons (Lex f s) =
    Identity $
      f s >>= \(t, s') -> Just (t, Lex f s')

countParen :: Hoas Token [Token]
countParen = many (char LP)

countParenP :: (Stream s Token) => Parser s [Token]
countParenP = makeParse countParen

{-

-}

-- lexStream :: Stream s t -> Parser s t ->
-- lexStream token s = _
