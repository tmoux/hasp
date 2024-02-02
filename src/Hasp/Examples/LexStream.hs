{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Hasp.Examples.LexStream where

import Data.Some
import Hasp.Parser (Parser, parse)
import Hasp.Stream

-- A datatype representing streams s tokenizing to t
data LexStream s t = Lex (s -> Maybe (Some t, s)) s

instance Stream (LexStream s (Token t)) t where
  uncons (Lex f s) =
    f s >>= \(t, s') -> Just (t, Lex f s')

makeLexStream :: (Stream s t0) => Parser s (Some (Token t)) -> s -> LexStream s (Token t)
makeLexStream p = Lex (parse p)
