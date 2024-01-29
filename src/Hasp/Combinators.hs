module Hasp.Combinators where

import Control.Applicative
import Hasp.Hoas
import Prelude hiding (seq)

option :: Hoas a -> Hoas (Maybe a)
option p = Just <$> p <|> eps Nothing

between :: Hoas a -> Hoas b -> Hoas c -> Hoas c
between open close p = open *> p <* close

paren :: Hoas a -> Hoas a
paren = between (char '(') (char ')')

star :: Hoas a -> Hoas [a]
star p = fix $
  \rest -> eps [] <|> (:) <$> p <*> rest

charset :: [Char] -> Hoas Char
charset l = asum (char <$> l)

-- TODO: Note order of fold
-- Or, it shouldn't really matter due to unambiguity
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

-- TODO: this is probably inefficient
digit :: Hoas Int
digit = read . return <$> charset ['0' .. '9']

chainr1 :: Hoas a -> Hoas (a -> a -> a) -> Hoas a
chainr1 a op = fix $
  \p ->
    let rest = option $ (flip <$> op) <*> p
     in f <$> a <*> rest
  where
    f x Nothing = x
    f x (Just g) = g x

chainr :: Hoas a -> Hoas (a -> a -> a) -> a -> Hoas a
chainr a op def = chainr1 a op <|> eps def

chainl1 :: Hoas a -> Hoas (a -> a -> a) -> Hoas a
chainl1 a op = reassoc <$> a <*> star (seq op a)
  where
    reassoc = foldl (\acc (f, y) -> acc `f` y)

chainl :: Hoas a -> Hoas (a -> a -> a) -> a -> Hoas a
chainl a op def = chainl1 a op <|> eps def
