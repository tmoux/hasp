module Hasp.Examples.Dyck where

import Hasp.Char
import Hasp.Combinators
import Hasp.Examples.Parsers (makeParser)
import Hasp.Hoas
import Hasp.Parser (parse)
import Hasp.Stream

dyck :: Hoas CharTag Int
dyck = fix $ \p -> eps 0 <|> (\x y -> x + y + 1) <$> between (char '(') (char ')') p <*> p

-- Invalid parser:
-- dyck = fix $ \p -> eps 0 <|> (\x y -> x + y + 1) <$> p <*> between (char '(') (char ')') p

countParens :: (Stream s CharTag) => s -> Maybe Int
countParens s = fst <$> parse (makeParser dyck) s
