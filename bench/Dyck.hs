{-# LANGUAGE FlexibleContexts #-}

module Dyck where

import Data.Text (Text)
import qualified Data.Text as T
import Hasp.Char (CharTag)
import Hasp.Stream
import Text.Parsec (Parsec, between, char, parse, (<|>))
import Text.Parsec.Text (Parser)

dyck :: Parsec String () Int
dyck = (\x y -> x + y + 1) <$> between (char '(') (char ')') dyck <*> dyck <|> return 0

-- Invalid parser:
-- dyck = (\x y -> x + y + 1) <$> dyck <*> between (char '(') (char ')') dyck <|> return 0

dyck' :: Parser Int
dyck' = (\x y -> x + y + 1) <$> between (char '(') (char ')') dyck' <*> dyck' <|> return 0

countParens :: Text -> Maybe Int
countParens s = case parse dyck' "" s of
  Left _ -> Nothing
  Right i -> Just i

handWritten :: Text -> Maybe Int
handWritten = (fst <$>) . expP

-- E = ( E ) | eps
expP :: Text -> Maybe (Int, Text)
expP s = case T.uncons s of
  Nothing -> return (0, s)
  Just (c1, s') ->
    if c1 == '('
      then do
        (inner, s'') <- expP s'
        (c2, s''') <- T.uncons s''
        if c2 == ')'
          then (\(x, rest) -> (x + inner + 1, rest)) <$> expP s'''
          else Nothing
      else return (0, s)

handWrittenCharTag :: Text -> Maybe Int
handWrittenCharTag = (fst <$>) . expP2

expP2 :: (Stream s CharTag) => s -> Maybe (Int, s)
expP2 s = case unconsTag s of
  Nothing -> return (0, s)
  Just (c1, s') ->
    if c1 == '('
      then do
        (inner, s'') <- expP2 s'
        (c2, s''') <- unconsTag s''
        if c2 == ')'
          then (\(x, rest) -> (x + inner + 1, rest)) <$> expP2 s'''
          else Nothing
      else return (0, s)
