module Parsers where

import Control.Applicative
import Hasp.Combinators
import Hasp.Hoas

data Sexp = Sym Char | SSeq [Sexp]
  deriving (Show, Eq)

letter :: Hoas Char
letter = charset ['a' .. 'z']

word :: Hoas [Char]
word = star letter

sexp :: Hoas Sexp
sexp = fix $
  \p -> Sym <$> letter <|> SSeq <$> paren (star p)

hEps :: Hoas ()
hEps = eps ()

hParens :: Hoas Char
hParens = paren (char 'c')

hAlt :: Hoas Char
hAlt = char 'c' <|> char 'd'

hStarParen :: Hoas [Char]
hStarParen = star $ paren $ char 'c'

hSexpChar :: Hoas (Sexp, Char)
hSexpChar = (,) <$> sexp <*> letter

hMultiSexp :: Hoas [Sexp]
hMultiSexp = star sexp

hBadFixpoint :: Hoas Char
hBadFixpoint = fix (\p -> eps 'c' <|> p *> char 'c')

hBadDisj :: Hoas Char
hBadDisj = char 'c' <|> char 'c'

hDyck :: Hoas Int
hDyck = fix $ \p -> eps 0 <|> (\a b -> a + b + 1) <$> paren p <*> p

data E
  = C Int
  | (:+:) E E
  deriving (Show, Eq)

infixl 9 :+:

hAddsR :: Hoas E
hAddsR = chainr1 (C <$> digit) ((:+:) <$ char '+')

hAddsL :: Hoas E
hAddsL = chainl1 (C <$> digit) ((:+:) <$ char '+')
