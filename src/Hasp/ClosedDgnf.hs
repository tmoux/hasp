{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}

module Hasp.ClosedDgnf where

import Data.Dependent.Map ((!))
import qualified Data.Dependent.Map as DM
import Data.GADT.Compare (GCompare)
import Hasp.Normalization
import qualified Hasp.Resolved as R
import Prelude hiding (null)

data NonTerminal m t a = NonTerminal
  { _closed_productions :: DM.DMap t (DGNFProd m a),
    _closed_null :: Maybe a
  }

instance Functor (NonTerminal m t) where
  fmap f (NonTerminal prods null) = NonTerminal (DM.map (fmapDGNFProd f) prods) (f <$> null)

data Grammar m t a = Grammar
  { _closed_start :: NonTerminalId m a,
    _closed_nonterminals :: DM.DMap (NonTerminalId m) (NonTerminal m t)
  }

convertToClosed :: forall m t a. (GCompare t) => DGNFGrammar m '[] t a -> Grammar m t a
convertToClosed (DGNFGrammar start nonterms) =
  Grammar start (DM.map convertNT nonterms)
  where
    convertNT :: DGNFNonTerminal m '[] t v -> NonTerminal m t v
    convertNT (DGNFNonTerminal prods null) = NonTerminal (DM.mapKeysMonotonic convertNF prods) null

    convertNF :: NF '[] t c -> t c
    convertNF (Term t) = t
    convertNF (NFVar v) = case v of {}

-- Next, convert these into the Resolved type.
-- this involves resolving the NonTerminalIds to NonTerminals.

resolve :: Grammar m t a -> R.NonTerminal t a
resolve (Grammar start nonterms) =
   resolveNonTerm nonterms (nonterms ! start)

resolveNonTerm ::
  DM.DMap (NonTerminalId m) (NonTerminal m t) ->
  NonTerminal m t a ->
  R.NonTerminal t a
resolveNonTerm env (NonTerminal prods null) =
  let prods' = DM.map (resolveProd env) prods
   in R.NonTerminal prods' null

resolveProd ::
  DM.DMap (NonTerminalId m) (NonTerminal m t) ->
  DGNFProd m a b ->
  R.Prod t a b
resolveProd env (DGNFProd ntseq f) =
  let ntseq' = resolveNTSeq env ntseq
   in R.Prod ntseq' f

resolveNTSeq ::
  DM.DMap (NonTerminalId m) (NonTerminal m t) ->
  DGNFNTSeq m a ->
  R.NTSeq t a
resolveNTSeq _ (Nil a) = R.Nil a
resolveNTSeq env (Cons n ns f) =
   R.Cons (resolveNonTerm env (env ! n)) (resolveNTSeq env ns) f
