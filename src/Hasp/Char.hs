module Hasp.Char where

import Hasp.Combinators
import Hasp.Hoas
import Hasp.Stream

type CharTag = Tag Char

char :: Char -> Hoas CharTag Char
char c = tok (Tag c)

range :: Char -> Char -> [CharTag Char]
range lo hi = Tag <$> [lo .. hi]

-- TODO: this is probably inefficient
digit :: Hoas CharTag Int
digit = read . return <$> charset (range '0' '9')

alpha :: Hoas CharTag Char
alpha = charset $ range 'a' 'z' ++ range 'A' 'Z'

alphaNum :: Hoas CharTag Char
alphaNum = charset $ range 'a' 'z' ++ range 'A' 'Z' ++ range '0' '9'

paren :: Hoas CharTag a -> Hoas CharTag a
paren = between (char '(') (char ')')

-- Note: we can't write a satisfy function that takes in an arbitrary predicate, so we can't reuse Data.Char.isSpace
space :: Hoas CharTag Char
space = charset $ Tag <$> [' ', '\t', '\n', '\r', '\f', '\v']
