module Hasp.Char where

import Hasp.Combinators
import Hasp.Hoas

-- TODO: this is probably inefficient
digit :: Hoas Char Int
digit = read . return <$> charset ['0' .. '9']

alpha :: Hoas Char Char
alpha = charset $ ['a' .. 'z'] ++ ['A' .. 'Z']

alphaNum :: Hoas Char Char
alphaNum = charset $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']

paren :: Hoas Char a -> Hoas Char a
paren = between (char '(') (char ')')

-- Note: we can't write a satisfy function that takes in an arbitrary predicate, so we can't reuse Data.Char.isSpace
space :: Hoas Char Char
space = charset [' ', '\t', '\n', '\r', '\f', '\v']

