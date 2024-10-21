module Hasp.DgnfDebug where

import Data.Dependent.Map ((!))
import qualified Data.Dependent.Map as DM
import Data.Dependent.Sum (DSum (..))
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

data Grammar t = Grammar Int [NonTerminal t]
  deriving (Show)

-- type EnvState m = State (M.Map (Some (NonTerminalId m)) Int, Int)

-- lookupRef :: NonTerminalId m a -> State (M.Map (Some (NonTerminalId m)) Int, Int) Int
-- lookupRef i = do
--   (mp, idx) <- get
--   case M.lookup (mkSome i) mp of
--     Just x -> return x
--     Nothing -> do
--       put (M.insert (mkSome i) idx mp, idx + 1)
--       return idx

-- Holder type for (index, NonTerminal)
data Foo m t a = Foo Int (C.NonTerminal m t a)

getIndex :: NonTerminalId m a -> DM.DMap (NonTerminalId m) (Foo m t) -> Int
getIndex i mp = let Foo j _ = mp ! i in j

convertDebug :: C.Grammar m t a -> Grammar t
convertDebug (C.Grammar start nonterms) = Grammar (getIndex start env) nonTermList
  where
    nonTermList = map (\(_ :=> v) -> convertNonTerm env v) (DM.assocs nonterms)
    env = DM.fromList $ zipWith (\i (k :=> v) -> k :=> Foo i v) [0 ..] (DM.assocs nonterms)

convertNonTerm ::
  DM.DMap (NonTerminalId m) (Foo m t) ->
  C.NonTerminal m t a ->
  NonTerminal t
convertNonTerm env (C.NonTerminal prods null) =
  let productions = map (\(k :=> v) -> (mkSome k, convertProd env v)) (DM.assocs prods)
   in NonTerminal productions (isJust null)

convertProd ::
  DM.DMap (NonTerminalId m) (Foo m t) ->
  DGNFProd m a b ->
  Prod
convertProd env (DGNFProd ntseq _) = Prod $ convertNTSeq env ntseq

convertNTSeq ::
  DM.DMap (NonTerminalId m) (Foo m t) ->
  DGNFNTSeq m a ->
  [Int]
convertNTSeq env s = case s of
  Nil _ -> []
  Cons n ns _ -> getIndex n env : convertNTSeq env ns
