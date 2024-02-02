module Parsers where

import Control.Monad.Except (runExcept)
import Data.GADT.Compare
import Data.GADT.Show
import Hasp.Char
import Hasp.Combinators
import Hasp.Hoas
import Hasp.Parser (Parser, toParser)
import Hasp.Stream
import Hasp.Typecheck (typecheck)

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

makeParser :: (Stream s t, GEq t, GShow t, GCompare t) => Hoas t a -> Parser s a
makeParser p = case runExcept $ toParser <$> typecheck (toTerm p) of
  Left err -> error err
  Right parser -> parser
