{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Hasp.Normalization where

import Control.Monad.Primitive (PrimMonad (PrimState))
import Data.Dependent.Map ((!))
import qualified Data.Dependent.Map as DM
import Data.Kind (Type)
import Data.Unique.Tag
import Debug.Todo (todo_)
import Hasp.Ctx (Index (..))
import Hasp.Grammar (Grammar, Grammar' (..))
import Prelude hiding (null)

-- Type of DGNF:
-- A DGNF normal form is either an epsilon, a terminal followed by several nonterminals (t n_1 n_2 ...)
-- or a fresh variable alpha followed by several nonterminals (a n_1 n_2 ...)
-- We use Debrujin indices for alpha, so they need to carry a Context / current depth in the type.
-- A closed form well-typed term will normalize to a DGNF without any free variables.
-- Note: the (parsing) types aren't directly used in the normalization process, but they guarantee that the result is well-defined, so it should be included in the exposed interface.
-- Each production should have a semantic action associated with it.

-- A NonTerminalId is indexed with the type of the parsing result that we get from parsing
-- starting at this nonterminal.
type NonTerminalId m = Tag (PrimState m)

-- Represents a sequence of nonterminals n_1 n_2 ... n_i and the associated semantic actions
data DGNFNTSeq :: (Type -> Type) -> (Type -> Type) -> Type -> Type where
  Nil :: a -> DGNFNTSeq m t a
  Cons :: NonTerminalId m b -> DGNFNTSeq m t c -> (b -> c -> a) -> DGNFNTSeq m t a

data DGNFProd :: (Type -> Type) -> (Type -> Type) -> Type -> Type -> Type where
  DGNFProd :: DGNFNTSeq m t c -> (b -> c -> a) -> DGNFProd m t a b

data NF ctx t a = Term (t a) | NFVar (Index ctx a)

-- instance (GEq t) => GEq (NF ctx t) where
--   geq = todo_
--
-- instance (GCompare t) => GCompare (NF ctx t) where
--   gcompare = todo_

data DGNFNonTerminal m ctx t a where
  DGNFNonTerminal :: DM.DMap (NF ctx t) (DGNFProd m t a) -> Maybe a -> DGNFNonTerminal m ctx t a

-- Is this instance needed?
-- instance Functor (DGNFNTSeq m t) where
--   fmap f (Nil v) = Nil (f v)
--   fmap f (Cons n ns g) = Cons n ns ((f .) . g)
--

-- awkward to write a Functor instance
fmapDGNFProd :: (a -> d) -> DGNFProd m t a b -> DGNFProd m t d b
fmapDGNFProd f (DGNFProd ns g) = DGNFProd ns ((f .) . g)

--
instance Functor (DGNFNonTerminal m ctx t) where
   fmap f (DGNFNonTerminal prods null) = DGNFNonTerminal (DM.map (fmapDGNFProd f) prods) (f <$> null)


epsNonTerminal :: a -> DGNFNonTerminal m ctx t a
epsNonTerminal a = DGNFNonTerminal DM.empty (Just a)

tokenNonTerminal :: t a -> DGNFNonTerminal m ctx t a
tokenNonTerminal tok = DGNFNonTerminal (DM.singleton (Term tok) prod) Nothing
  where
    prod :: DGNFProd m t a a
    prod = DGNFProd (Nil ()) const

varNonTerminal :: Index ctx a -> DGNFNonTerminal m ctx t a
varNonTerminal idx = DGNFNonTerminal (DM.singleton (NFVar idx) prod) Nothing
  where
    prod :: DGNFProd m t a a
    prod = DGNFProd (Nil ()) const

data DGNFGrammar m ctx t a = DGNFGrammar
  { _start :: NonTerminalId m a,
    _nonterminals :: DM.DMap (NonTerminalId m) (DGNFNonTerminal m ctx t)
  }

normalize :: (PrimMonad m) => Grammar '[] t a d -> m (DGNFGrammar m '[] t a)
normalize = normalize'

normalize' :: (PrimMonad m) => Grammar ctx t a d -> m (DGNFGrammar m ctx t a)
normalize' (gr, _) =
  newTag >>= \n -> case gr of
    Eps a -> return $ DGNFGrammar n (DM.singleton n (epsNonTerminal a))
    Tok t -> return $ DGNFGrammar n (DM.singleton n (tokenNonTerminal t))
    Bot -> return $ DGNFGrammar n DM.empty
    Seq a b -> todo_
    Alt a b -> todo_
    Fix g -> todo_
    Map f x -> do
      DGNFGrammar n' x' <- normalize' x
      return $ DGNFGrammar n (DM.insert n (f <$> (x' ! n')) x')
    Var x -> return $ DGNFGrammar n (DM.singleton n (varNonTerminal x))

{-
data NF :: [Type] -> (Type -> Type) -> Type where
  EpsProd :: NF ctx t
  TerminalProd :: t a -> [NonTerminalId] -> NF ctx t
  VarProd :: Index ctx a -> [NonTerminalId] -> NF ctx t

type ClosedNF t = NF '[] t

-- Eliminate from a ClosedNF (only consider EpsProd, TerminalProd cases)
elimClosedNF ::
  r ->
  (forall a. t a -> [NonTerminalId] -> r) ->
  ClosedNF t ->
  r
elimClosedNF fEps fTerm = \case
  EpsProd -> fEps
  TerminalProd t ns -> fTerm t ns
  VarProd t _ -> case t of {}

instance (Show (Some t)) => Show (NF ctx t) where
  show EpsProd = "eps"
  show (TerminalProd t ns) = show (Some t) ++ " " ++ show ns
  show (VarProd a ns) = show a ++ " " ++ show ns

-- NOTE: we need to add data for actual parsers
data DGNF :: [Type] -> (Type -> Type) -> Type where
  DGNF :: NonTerminalId -> M.Map NonTerminalId [NF ctx t] -> DGNF ctx t

data DGNF' :: [Type] -> (Type -> Type) -> Type where
  DGNF' :: NonTerminalId -> M.Map NonTerminalId [NF ctx t] -> DGNF' ctx t

-- Top-level normalization function

-- normalize :: forall t a. Grammar '[] t a (Tp (Some t)) -> DGNF '[] t
-- TODO: relabel/recanonicalize the labels?
-- TODO: detect isomorphic nonterminals? bisimulation?
-- This is made more difficult by productions.
normalize :: forall ctx t a d. (Show (Some t)) => Grammar ctx t a d -> DGNF ctx t
normalize g = prune $ eval d 0
  where
    d :: State NonTerminalId (DGNF ctx t)
    d = normalize' g

prune :: DGNF ctx t -> DGNF ctx t
prune (DGNF n g) = DGNF n (M.filterWithKey (\k _ -> S.member k reachableNonterminals) g)
  where
    reachableNonterminals :: S.Set NonTerminalId
    reachableNonterminals = dfs S.empty n

    dfs :: S.Set NonTerminalId -> NonTerminalId -> S.Set NonTerminalId
    dfs seen nid
      | S.member nid seen = S.empty
      | otherwise = S.insert nid (S.unions (map f (g ! nid)))
      where
        seen' = S.insert nid seen
        f :: NF ctx t -> S.Set NonTerminalId
        f EpsProd = S.empty
        f (TerminalProd _ ns) = S.union (S.fromList ns) (S.unions (map (dfs seen') ns))
        f (VarProd _ ns) = S.union (S.fromList ns) (S.unions (map (dfs seen') ns))

-- Helper normalization function
-- TODO: Maybe we don't use DGNF here to reduce wrapping/unwrapping?
-- normalize' :: (MonadFresh m) => Grammar ctx t a d -> m (DGNF ctx t)
normalize' :: (MonadFresh m, Show (Some t)) => Grammar ctx t a d -> m (DGNF ctx t)
normalize' (gr, _) =
  genFresh >>= \n -> case gr of
    (Eps _) -> return $ DGNF n (M.singleton n [EpsProd])
    (Tok t) -> return $ DGNF n (M.singleton n [TerminalProd t []])
    Bot -> return $ DGNF n M.empty
    (Seq a b) -> do
      DGNF n1 g1 <- normalize' a
      DGNF n2 g2 <- normalize' b
      let adds = catMaybes [append ns1 [n2] | ns1 <- g1 ! n1]
      return $ DGNF n (M.singleton n adds <+> g1 <+> g2)
    (Alt a b) -> do
      DGNF n1 g1 <- normalize' a
      DGNF n2 g2 <- normalize' b
      return $ DGNF n (M.singleton n (g1 ! n1 ++ g2 ! n2) <+> g1 <+> g2)
    Fix g -> do
      DGNF n' g' <- normalize' g

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

      -- traceM ("n':" ++ show n')
      -- traceM ("g':" ++ show g')
      -- traceM ("beginWithVar:" ++ show beginWithVar)
      -- traceM ("originalProds:" ++ show originalProds)
      -- traceM (show g')
      return $
        DGNF
          n
          ( M.singleton n originalProds
              <+> beginWithVar
              <+> notBeginWithVar
          )
    (Var x) -> return $ DGNF n (M.singleton n [VarProd x []])
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
-}
