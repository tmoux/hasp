{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Use const" #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Hasp.Normalization where

import Control.Monad.State.Strict (State, evalState, state)
import Data.Kind (Type)
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, mapMaybe)
import Data.Some (Some (Some))
import Debug.Trace
import Hasp.Ctx (Index (..))
import Hasp.Grammar (Grammar, Grammar' (..))
import Hasp.Types (Tp)

-- Type of DGNF:
-- A DGNF normal form is either an epsilon, a terminal followed by several nonterminals (t n_1 n_2 ...)
-- or a fresh variable alpha followed by several nonterminals (a n_1 n_2 ...)
-- We use Debrujin indices for alpha, so they need to carry a Context / current depth in the type.
-- A closed form well-typed term will normalize to a DGNF without any free variables.
-- Note: the types aren't directly used in the normalization process, but they guarantee that the result is well-defined, so it should be included in the exposed interface.

type NonTerminalId = Int

-- Monad class for running normalization (we need to generate fresh nonterminal ids)
class (Monad m) => MonadFresh m where
  genFresh :: m NonTerminalId
  eval :: m a -> NonTerminalId -> a

instance MonadFresh (State NonTerminalId) where
  genFresh = state (\x -> (x, x + 1))
  eval = evalState

data NF :: [Type] -> (Type -> Type) -> Type where
  EpsProd :: NF ctx t
  TerminalProd :: t a -> [NonTerminalId] -> NF ctx t
  VarProd :: Index ctx a -> [NonTerminalId] -> NF ctx t

instance (Show (Some t)) => Show (NF ctx t) where
  show EpsProd = "eps"
  show (TerminalProd t ns) = show (Some t) ++ " " ++ show ns
  show (VarProd a ns) = show a ++ " " ++ show ns

-- NOTE: we need to add data for actual parsers
data DGNF :: [Type] -> (Type -> Type) -> Type where
  DGNF :: (NonTerminalId -> M.Map NonTerminalId [NF ctx t]) -> DGNF ctx t

unDGNF :: DGNF ctx t -> (NonTerminalId -> M.Map NonTerminalId [NF ctx t])
unDGNF (DGNF f) = f

-- Top-level normalization function

-- normalize :: forall t a. Grammar '[] t a (Tp (Some t)) -> DGNF '[] t
normalize :: forall ctx t a d. (Show (Some t)) => Grammar ctx t a d -> DGNF ctx t
normalize g = eval d 0
  where
    d :: State NonTerminalId (DGNF ctx t)
    d = normalize' g

-- Helper normalization function
-- TODO: Maybe we don't use DGNF here to reduce wrapping/unwrapping?
-- normalize' :: (MonadFresh m) => Grammar ctx t a d -> m (DGNF ctx t)
normalize' :: (MonadFresh m, Show (Some t)) => Grammar ctx t a d -> m (DGNF ctx t)
normalize' (gr, _) = case gr of
  (Eps _) -> return $ DGNF (\n -> M.singleton n [EpsProd])
  (Tok t) -> return $ DGNF (\n -> M.singleton n [TerminalProd t []])
  Bot -> return $ DGNF (\_ -> M.empty)
  (Seq a b) -> do
    n1 <- genFresh
    g1 <- unDGNF <$> normalize' a <*> pure n1
    n2 <- genFresh
    g2 <- unDGNF <$> normalize' b <*> pure n2
    let adds = catMaybes [append ns1 [n2] | ns1 <- g1 ! n1]
    return $ DGNF (\n -> M.singleton n adds <+> g1 <+> g2)
  (Alt a b) -> do
    n1 <- genFresh
    g1 <- unDGNF <$> normalize' a <*> pure n1
    n2 <- genFresh
    g2 <- unDGNF <$> normalize' b <*> pure n2
    return $ DGNF (\n -> M.singleton n (g1 ! n1 ++ g2 ! n2) <+> g1 <+> g2)
  Fix g -> do
    n' <- genFresh
    g' <- unDGNF <$> normalize' g <*> pure n'

    -- Type 1
    let originalProds = map shift (g' ! n')

    -- Type 2
    -- TODO: rename these helper functions
    -- traceM $ "original prods: " ++ show originalProds
    let fn :: NF (a : ctx) t -> NF ctx t -> Maybe (NF ctx t)
        fn nf1 nf2 = case nf1 of
          (VarProd IndexZ ns) -> append nf2 ns
          _ -> Nothing

        beginWithVar =
          -- trace ("original prods: " ++ show originalProds) $
            M.map (concatMap (\nf -> mapMaybe (fn nf) originalProds)) g'

    -- Type 3
    -- TODO: rename these helper functions
    let f :: NF (a : ctx) t -> Maybe (NF ctx t)
        f nf = case nf of
          EpsProd -> Just EpsProd
          (TerminalProd t ns) -> Just $ TerminalProd t ns
          (VarProd IndexZ _) -> Nothing
          (VarProd (IndexS x) ns) -> Just $ VarProd x ns

        notBeginWithVar = M.map (mapMaybe f) g'

    traceM ("n':" ++ show n')
    traceM ("g':" ++ show g')
    traceM ("beginWithVar:" ++ show beginWithVar)
    traceM ("originalProds:" ++ show originalProds)
    -- traceM (show g')
    return $
      DGNF
        ( \n ->
            M.singleton n originalProds
              <+> beginWithVar
              <+> notBeginWithVar
        )
  (Var x) -> return $ DGNF (\n -> M.singleton n [VarProd x []])
  (Map _ x) -> normalize' x
  where
    (<+>) :: (Ord k) => M.Map k [a] -> M.Map k [a] -> M.Map k [a]
    (<+>) = M.unionWith (++)

shift :: NF (a : ctx) t -> NF ctx t
shift EpsProd = EpsProd
shift (TerminalProd t ns) = TerminalProd t ns
shift (VarProd IndexZ _) = error "unreachable! shift"
shift (VarProd (IndexS x) ns) = VarProd x ns

append :: NF ctx t -> [NonTerminalId] -> Maybe (NF ctx t)
append nf [] = Just nf
append nf c = case nf of
  EpsProd -> Nothing
  (TerminalProd t ns) -> Just $ TerminalProd t (ns ++ c)
  (VarProd v ns) -> Just $ VarProd v (ns ++ c)
