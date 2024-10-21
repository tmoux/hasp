{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Hasp.CreateParser where

import Control.Monad.Except (runExcept)
import Control.Monad.ST (runST)
import Data.GADT.Compare (GCompare)
import Data.GADT.Show (GShow)
import Data.Some (Some)
import qualified Data.Text as T
import Hasp.ClosedDgnf (convertToClosed, resolve)
import Hasp.Combinators (many, between, choice)
import qualified Hasp.DgnfDebug as D
import Hasp.Grammar (Grammar)
import Hasp.Hoas (Hoas, toTerm, fix, tok)
import Hasp.Normalization (normalize)
import Hasp.Parser (Parser, parse)
import Hasp.Resolved (parserFromNT)
import Hasp.Stream (Stream, Tag (..))
import Hasp.Typecheck (typecheck)
import Hasp.Types (Tp)

convertDGNF :: (Stream s t, GShow t, GCompare t) => Grammar '[] t a (Tp (Some t)) -> Parser s a
convertDGNF g = runST $ parserFromNT . resolve . convertToClosed <$> normalize g

convertToDebug :: (GShow t, GCompare t) => Grammar '[] t a (Tp (Some t)) -> D.Grammar t
convertToDebug g = runST $ D.convertDebug . convertToClosed <$> normalize g

-- normalized <- normalize g
-- let closed = convertToClosed normalized
--     resolved = resolve closed
-- return (parserFromNT resolved)

makeParserDGNF :: (Stream s t, GShow t, GCompare t) => Hoas t a -> Parser s a
makeParserDGNF p = case parser of
  Left err -> error err
  Right result -> result
  where
    parser = runExcept $ do
      typechecked <- typecheck (toTerm p)
      return (convertDGNF typechecked)

makeDebug :: (GShow t, GCompare t) => Hoas t a -> D.Grammar t
makeDebug p = case parser of
  Left err -> error err
  Right result -> result
  where
    parser = runExcept $ do
      typechecked <- typecheck (toTerm p)
      return (convertToDebug typechecked)

-- parsing test
hoas :: Hoas (Tag Char) Int
-- hoas = (,) <$> tok (Tag 'a') <*> tok (Tag 'b')
-- hoas = (,) <$> tok (Tag 'a') <*> tok (Tag 'b') <* tok (Tag 'z')
-- hoas = (,) <$> (tok (Tag 'a') <|> tok (Tag 'b')) <*> tok (Tag 'z')
-- hoas = fix $ \p -> (\_ _ -> 1) <$> tok (Tag 'a') <*> p

-- hoas = fix $ \p ->
--   eps 0
--     <|> (\x y -> x + y + 1) <$> between (char '(') (char ')') p <*> p
-- hoas = fix $ \p ->
--   choice
--     [ (+ 1) <$ char 'a' <*> p,
--       1 <$ char 'b'
--     ]

-- Doesn't work:
-- Works now?
-- hoas = sum <$> many (1 <$ char 'a')

-- hoas = eps 0
-- Works:
-- hoas = fix $ \p ->
--   choice
--     [ between (tok (Tag '(')) (tok (Tag ')')) ((+ 1) <$> p),
--       1 <$ tok (Tag 'a')
--     ]

-- hoas = fix $ \p -> eps 0 <|> ((+ 1) <$ char 'a' <*> p)

-- hoas = ((+) <$> (1 <$ char 'a') <*> (2 <$ char 'b')) <|> eps 0

hoas = fix $ \p ->
  choice
    [ between (tok (Tag '(')) (tok (Tag ')')) (sum <$> many p),
      1 <$ tok (Tag 'a')
    ]

parser1 :: Parser T.Text Int
parser1 = makeParserDGNF hoas

r1 :: D.Grammar (Tag Char)
r1 = makeDebug hoas

s :: T.Text
-- s = "aaab"
s = "(a(a)(aa)(((a))))abc"

ans :: Maybe (Int, T.Text)
ans = parse parser1 s
