module Sexp where

-- Parse s-expressions with alphanumeric atoms

import Hasp.Hoas
import Hasp.Combinators

letter :: Hoas Char Char
letter = char 'a' <|> char 'b'