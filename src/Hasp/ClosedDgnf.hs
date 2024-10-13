{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}

module Hasp.ClosedDgnf where

import Control.Monad.Reader (MonadReader, runReader)
import Data.Dependent.Map ((!))
import qualified Data.Dependent.Map as DM
import Data.Dependent.Sum
import Data.GADT.Compare (GCompare)
import Debug.Todo (todo_)
import Hasp.Normalization
import qualified Hasp.Resolved as R
import Prelude hiding (null)

mapDMapKeys ::
  (GCompare k, GCompare k') =>
  (forall a. k a -> k' a) ->
  DM.DMap k f ->
  DM.DMap k' f
mapDMapKeys f = DM.fromList . map mapSome . DM.toList
  where
    mapSome (k :=> v) = f k :=> v

-- Conversion from DGNF to Resolved:

--  First, if the context is empty, NFs can only be terminals, not variables:

data NonTerminal m t a = NonTerminal
  { _closed_productions :: DM.DMap t (DGNFProd m t a),
    _closed_null :: Maybe a
  }

data Grammar m t a = Grammar
  { _closed_start :: NonTerminalId m a,
    _closed_nonterminals :: DM.DMap (NonTerminalId m) (NonTerminal m t)
  }

convertToClosed :: forall m t a. (GCompare t) => DGNFGrammar m '[] t a -> Grammar m t a
convertToClosed (DGNFGrammar start nonterms) =
  Grammar start (DM.map convertNT nonterms)
  where
    convertNT :: DGNFNonTerminal m '[] t v -> NonTerminal m t v
    convertNT (DGNFNonTerminal prods null) = NonTerminal (mapDMapKeys convertNF prods) null

    convertNF :: NF '[] t c -> t c
    convertNF (Term t) = t
    convertNF (NFVar v) = case v of {}

-- Next, convert these into the Resolved type.
-- this involves resolving the NonTerminalIds to NonTerminals.

resolve :: Grammar m t a -> R.NonTerminal t a
resolve = todo_

-- resolve (Grammar start nonterms) = runReader (resolveNonTerm (nonterms ! start)) todo_

resolveNonTerm ::
  (MonadReader (DM.DMap (NonTerminalId m) (NonTerminal m t)) mr) =>
  NonTerminal m t a ->
  mr (R.NonTerminal t a)
resolveNonTerm = todo_
