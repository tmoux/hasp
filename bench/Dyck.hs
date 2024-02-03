module Dyck where

import Text.Parsec

type Parser = Parsec String ()

lparen :: Parser Char
lparen = char '('

rparen :: Parser Char
rparen = char ')'

dyck :: Parser Int
dyck = (\x y -> x + y + 1) <$> between lparen rparen dyck <*> dyck <|> return 0

countParens :: String -> Maybe Int
countParens s = case parse dyck "" s of
  Left _ -> Nothing
  Right i -> Just i
