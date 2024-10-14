{-# LANGUAGE DataKinds #-}

module Hasp.CreateParser where

import Control.Monad.Except (runExcept)
import Control.Monad.ST (runST)
import Data.GADT.Compare (GCompare)
import Data.GADT.Show (GShow)
import Data.Some (Some)
import Hasp.ClosedDgnf (convertToClosed, resolve)
import Hasp.Grammar (Grammar)
import Hasp.Hoas (Hoas, toTerm)
import Hasp.Normalization (normalize)
import Hasp.Parser (Parser)
import Hasp.Resolved (parserFromNT)
import Hasp.Stream (Stream)
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
