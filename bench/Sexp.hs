{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Sexp where

-- Parsec parser for s-expressions with alphanumeric atoms

import Control.Monad (void)
import Text.Parsec
import Text.Parsec.Text
import Data.Text

lparen :: Parser ()
lparen = void (char '(') <* spaces

rparen :: Parser ()
rparen = void (char ')') <* spaces

atom :: Parser String
atom = (:) <$> letter <*> many alphaNum <* spaces

sexp :: Parser Int
sexp = 1 <$ atom <|> between lparen rparen (sum <$> many sexp)

countAtoms :: Text -> Maybe Int
countAtoms s = case parse sexp "" s of
  Left _ -> Nothing
  Right i -> Just i
