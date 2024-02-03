module Hasp.Combinators where

import Control.Applicative hiding ((<|>))
import Hasp.Hoas
import Prelude hiding (seq)

option :: Hoas t a -> Hoas t (Maybe a)
option p = Just <$> p <|> eps Nothing

between :: Hoas t a -> Hoas t b -> Hoas t c -> Hoas t c
between open close p = open *> p <* close

star :: Hoas t a -> Hoas t [a]
star p = fix $
  \rest -> eps [] <|> (:) <$> p <*> rest

sepBy1 :: Hoas t a -> Hoas t sep -> Hoas t [a]
sepBy1 p sep = (:) <$> p <*> star (sep *> p)

sepBy :: Hoas t a -> Hoas t sep -> Hoas t [a]
sepBy p sep = eps [] <|> sepBy1 p sep

charset :: [t a] -> Hoas t a
charset l = asum (tok <$> l)

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

some :: Hoas t a -> Hoas t [a]
some p = (:) <$> p <*> star p

many1 :: Hoas t a -> Hoas t [a]
many1 p = (:) <$> p <*> star p

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
