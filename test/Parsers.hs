module Parsers where

import Hasp.Char
import Hasp.Combinators
import Hasp.Hoas

data Sexp = Sym Char | SSeq [Sexp]
  deriving (Show, Eq)

letter :: Hoas CharTag Char
letter = charset $ range 'a' 'z'

word :: Hoas CharTag [Char]
word = star letter

sexp :: Hoas CharTag Sexp
sexp = fix $
  \p -> Sym <$> letter <|> SSeq <$> paren (star p)

hEps :: Hoas CharTag ()
hEps = eps ()

hParens :: Hoas CharTag Char
hParens = paren (char 'c')

hAlt :: Hoas CharTag Char
hAlt = char 'c' <|> char 'd'

hStarParen :: Hoas CharTag [Char]
hStarParen = star $ paren $ char 'c'

hSexpChar :: Hoas CharTag (Sexp, Char)
hSexpChar = (,) <$> sexp <*> letter

hMultiSexp :: Hoas CharTag [Sexp]
hMultiSexp = star sexp

hBadFixpoint :: Hoas CharTag Char
hBadFixpoint = fix (\p -> eps 'c' <|> p *> char 'c')

hBadDisj :: Hoas CharTag Char
hBadDisj = char 'c' <|> char 'c'

hDyck :: Hoas CharTag Int
hDyck = fix $ \p -> eps 0 <|> (\a b -> a + b + 1) <$> paren p <*> p

data E
  = C Int
  | (:+:) E E
  deriving (Show, Eq)

infixl 9 :+:

hAddsR :: Hoas CharTag E
hAddsR = chainr1 (C <$> digit) ((:+:) <$ char '+')

hAddsL :: Hoas CharTag E
hAddsL = chainl1 (C <$> digit) ((:+:) <$ char '+')
