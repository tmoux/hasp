module Dyck where

import Text.Parsec
import Text.Parsec.Text
import Data.Text

dyck :: Parsec String () Int
dyck = (\x y -> x + y + 1) <$> between (char '(') (char ')') dyck <*> dyck <|> return 0
-- Invalid parser:
-- dyck = (\x y -> x + y + 1) <$> dyck <*> between (char '(') (char ')') dyck <|> return 0

dyck' :: Parser Int
dyck' = (\x y -> x + y + 1) <$> between (char '(') (char ')') dyck' <*> dyck' <|> return 0

countParens :: String -> Maybe Int
countParens s = case parse dyck "" s of
  Left _ -> Nothing
  Right i -> Just i

countParens' :: Text -> Maybe Int
countParens' s = case parse dyck' "" s of
  Left _ -> Nothing
  Right i -> Just i
