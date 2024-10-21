module Hasp.DgnfDebug where

import Control.Monad.State
import qualified Data.Dependent.Map as DM
import Data.Dependent.Sum (DSum (..))
import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
import Data.Maybe (isJust)
import Data.Some (Some, mkSome)
import qualified Hasp.ClosedDgnf as C
import Hasp.Normalization (DGNFNTSeq (..), DGNFProd (..), NonTerminalId)
import Prelude hiding (null)

-- Representation of closed DGNF for debugging.
-- Leave out semantic actions.

newtype Prod = Prod [Int]
  deriving (Show)

data NonTerminal t = NonTerminal
  { _productions :: [(Some t, Prod)],
    _null :: Bool
  }
  deriving (Show)

data Grammar t = Grammar Int [(Int, NonTerminal t)]
  deriving (Show)

type EnvState m = State (M.Map (Some (NonTerminalId m)) Int, Int)

-- lookupRef :: NonTerminalId m a -> State (M.Map (Some (NonTerminalId m)) Int, Int) Int
lookupRef i = do
  (mp, idx) <- get
  case M.lookup (mkSome i) mp of
    Just x -> return x
    Nothing -> do
      put (M.insert (mkSome i) idx mp, idx + 1)
      return idx

convertDebug :: C.Grammar m t a -> Grammar t
convertDebug (C.Grammar start nonterms) = Grammar start' nontermList
  where
    mp = mapM (\(k :=> v) -> (,) <$> lookupRef k <*> convertNonTerm nonterms v) (DM.assocs nonterms)
    (nontermList, (refMap, _)) = runState mp (M.empty, 0)
    start' = refMap ! start



convertNonTerm ::
  DM.DMap (NonTerminalId m) (C.NonTerminal m t) ->
  C.NonTerminal m t a ->
  EnvState m (NonTerminal t)
convertNonTerm env (C.NonTerminal prods null) = do
  productions <-
    mapM
      (\(k :=> v) -> convertProd env v >>= \p -> return (mkSome k, p))
      (DM.assocs prods)
  return $ NonTerminal productions (isJust null)

convertProd ::
  DM.DMap (NonTerminalId m) (C.NonTerminal m t) ->
  DGNFProd m a b ->
  EnvState m Prod
convertProd env (DGNFProd ntseq _) = Prod <$> convertNTSeq env ntseq

convertNTSeq ::
  DM.DMap (NonTerminalId m) (C.NonTerminal m t) ->
  DGNFNTSeq m a ->
  EnvState m [Int]
convertNTSeq env s = case s of
  Nil _ -> return []
  Cons n ns _ -> (:) <$> lookupRef n <*> convertNTSeq env ns
