module Parsers where

import Control.Applicative
import Hasp.Combinators
import Hasp.Hoas

data Sexp = Sym Char | SSeq [Sexp]
  deriving (Show, Eq)

letter :: Hoas Char Char
letter = charset ['a' .. 'z']

word :: Hoas Char [Char]
word = star letter

sexp :: Hoas Char Sexp
sexp = fix $
  \p -> Sym <$> letter <|> SSeq <$> paren (star p)

hEps :: Hoas Char ()
hEps = eps ()

hParens :: Hoas Char Char
hParens = paren (char 'c')

hAlt :: Hoas Char Char
hAlt = char 'c' <|> char 'd'

hStarParen :: Hoas Char [Char]
hStarParen = star $ paren $ char 'c'

hSexpChar :: Hoas Char (Sexp, Char)
hSexpChar = (,) <$> sexp <*> letter

hMultiSexp :: Hoas Char [Sexp]
hMultiSexp = star sexp

hBadFixpoint :: Hoas Char Char
hBadFixpoint = fix (\p -> eps 'c' <|> p *> char 'c')

hBadDisj :: Hoas Char Char
hBadDisj = char 'c' <|> char 'c'

hDyck :: Hoas Char Int
hDyck = fix $ \p -> eps 0 <|> (\a b -> a + b + 1) <$> paren p <*> p

data E
  = C Int
  | (:+:) E E
  deriving (Show, Eq)

infixl 9 :+:

hAddsR :: Hoas Char E
hAddsR = chainr1 (C <$> digit) ((:+:) <$ char '+')

hAddsL :: Hoas Char E
hAddsL = chainl1 (C <$> digit) ((:+:) <$ char '+')
