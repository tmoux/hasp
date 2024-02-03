module Dyck where

import Text.Parsec

type Parser = Parsec String ()

dyck :: Parsec String () Int
dyck = (\x y -> x + y + 1) <$> between (char '(') (char ')') dyck <*> dyck <|> return 0
-- Invalid parser:
-- dyck = (\x y -> x + y + 1) <$> dyck <*> between (char '(') (char ')') dyck <|> return 0

countParens :: String -> Maybe Int
countParens s = case parse dyck "" s of
  Left _ -> Nothing
  Right i -> Just i
