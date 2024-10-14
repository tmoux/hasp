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
import Hasp.Grammar (Grammar)
import Hasp.Hoas (Hoas, eps, toTerm, tok)
import Hasp.Normalization (normalize)
import Hasp.Parser (Parser)
import Hasp.Resolved (parserFromNT)
import Hasp.Stream (Stream, Tag (Tag))
import Hasp.Typecheck (typecheck)
import Hasp.Types (Tp)

convertDGNF :: (Stream s t, GShow t, GCompare t) => Grammar '[] t a (Tp (Some t)) -> Parser s a
convertDGNF g = runST $ parserFromNT . resolve . convertToClosed <$> normalize g

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

-- parsing test
hoas :: Hoas (Tag Char) (Char, Char)
-- hoas = (,) <$> tok (Tag 'a') <*> tok (Tag 'b')
hoas = (,) <$> tok (Tag 'a') <*> tok (Tag 'b') <* tok (Tag 'z')

parser1 :: Parser T.Text (Char, Char)
parser1 = makeParserDGNF hoas

s :: T.Text
s = "abza"
