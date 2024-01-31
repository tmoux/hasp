module Hasp.Char where

import Hasp.Hoas
import Hasp.Combinators

-- TODO: this is probably inefficient
digit :: Hoas Char Int
digit = read . return <$> charset ['0' .. '9']

paren :: Hoas Char a -> Hoas Char a
paren = between (char '(') (char ')')