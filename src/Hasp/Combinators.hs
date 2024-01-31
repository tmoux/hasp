module Hasp.Combinators where

import Control.Applicative
import Hasp.Hoas
import Prelude hiding (seq)

 -- TODO: Reorganize Char parsers into separate folders

option :: Hoas t a -> Hoas t (Maybe a)
option p = Just <$> p <|> eps Nothing

between :: Hoas t a -> Hoas t b -> Hoas t c -> Hoas t c
between open close p = open *> p <* close

paren :: Hoas Char a -> Hoas Char a
paren = between (char '(') (char ')')

star :: Hoas t a -> Hoas t [a]
star p = fix $
  \rest -> eps [] <|> (:) <$> p <*> rest

charset :: [t] -> Hoas t t
charset l = asum (char <$> l)

-- TODO: Note order of fold
-- Or, it shouldn't really matter due to unambiguity
choice :: [Hoas t a] -> Hoas t a
choice = asum

count :: Int -> Hoas t a -> Hoas t [a]
count n p
  | n <= 0 = eps []
  | otherwise = (:) <$> p <*> count (n - 1) p

many :: Hoas t a -> Hoas t [a]
many = star

many1 :: Hoas t a -> Hoas t [a]
many1 p = (:) <$> p <*> star p

-- TODO: this is probably inefficient
digit :: Hoas Char Int
digit = read . return <$> charset ['0' .. '9']

chainr1 :: Hoas t a -> Hoas t (a -> a -> a) -> Hoas t a
chainr1 a op = fix $
  \p ->
    let rest = option $ (flip <$> op) <*> p
     in f <$> a <*> rest
  where
    f x Nothing = x
    f x (Just g) = g x

chainr :: Hoas t a -> Hoas t (a -> a -> a) -> a -> Hoas t a
chainr a op def = chainr1 a op <|> eps def

chainl1 :: Hoas t a -> Hoas t (a -> a -> a) -> Hoas t a
chainl1 a op = reassoc <$> a <*> star (seq op a)
  where
    reassoc = foldl (\acc (f, y) -> acc `f` y)

chainl :: Hoas t a -> Hoas t (a -> a -> a) -> a -> Hoas t a
chainl a op def = chainl1 a op <|> eps def
