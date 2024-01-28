module Hasp.Combinators where

import Control.Applicative
import Hasp.Hoas

-- Parse a sequence of c's and return the length
parseList :: Char -> Hoas Int
parseList c = fix $
  \p -> eps 0 <|> ((+ 1) <$> (chr c *> p))

between :: Hoas a -> Hoas b -> Hoas c -> Hoas c
between open close p = open *> p <* close

-- Parse with parentheses
paren :: Hoas a -> Hoas a
paren = between (chr '(') (chr ')')

-- Yay operators are fun...
star :: Hoas a -> Hoas [a]
star p = fix $
  \rest -> eps [] <|> (:) <$> p <*> rest

charset :: [Char] -> Hoas Char
charset l = asum (chr <$> l)

choice :: [Hoas a] -> Hoas a
choice = asum

count :: Int -> Hoas a -> Hoas [a]
count n p
  | n <= 0 = eps []
  | otherwise = (:) <$> p <*> count (n - 1) p

many :: Hoas a -> Hoas [a]
many = star

many1 :: Hoas a -> Hoas [a]
many1 p = (:) <$> p <*> star p
