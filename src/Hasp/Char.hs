{-# LANGUAGE GADTs #-}

module Hasp.Char where

import Data.Data ((:~:) (..))
import Data.GADT.Compare
import Hasp.Combinators
import Hasp.Hoas

data CharTag c where
  CharTag :: Char -> CharTag Char

instance GEq CharTag where
  (CharTag a) `geq` (CharTag b)
    | a == b = Just Refl
    | otherwise = Nothing

range :: Char -> Char -> [CharTag Char]
range lo hi = CharTag <$> [lo .. hi]

-- TODO: this is probably inefficient
digit :: Hoas CharTag Int
digit = read . return <$> charset (range '0' '9')

alpha :: Hoas CharTag Char
alpha = charset $ range 'a' 'z' ++ range 'A' 'Z'

alphaNum :: Hoas CharTag Char
alphaNum = charset $ range 'a' 'z' ++ range 'A' 'Z' ++ range '0' '9'

paren :: Hoas CharTag a -> Hoas CharTag a
paren = between (char (CharTag '(')) (char (CharTag ')'))

-- Note: we can't write a satisfy function that takes in an arbitrary predicate, so we can't reuse Data.Char.isSpace
space :: Hoas CharTag Char
space = charset $ CharTag <$> [' ', '\t', '\n', '\r', '\f', '\v']
