{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Hasp.Normalization where

import Control.Applicative ((<|>))
import Control.Monad.Primitive (PrimMonad (PrimState))
-- import Data.Dependent.Map ((!))
import qualified Data.Dependent.Map as DM
import Data.Dependent.Sum (DSum (..))
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Unique.Tag
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
data DGNFNTSeq :: (Type -> Type) -> Type -> Type where
  Nil :: a -> DGNFNTSeq m a
  Cons :: NonTerminalId m b -> DGNFNTSeq m c -> (b -> c -> a) -> DGNFNTSeq m a

data DGNFProd :: (Type -> Type) -> Type -> Type -> Type where
  DGNFProd :: DGNFNTSeq m c -> (b -> c -> a) -> DGNFProd m a b

mapDGNFProd :: (a -> r) -> DGNFProd m a b -> DGNFProd m r b
mapDGNFProd f (DGNFProd ns g) = DGNFProd ns ((f .) . g)

data NF ctx t a = Term (t a) | NFVar (Index ctx a)

instance (GEq t) => GEq (NF ctx t) where
  (Term a) `geq` (Term b) = a `geq` b
  (NFVar a) `geq` (NFVar b) = a `geq` b
  _ `geq` _ = Nothing

instance (GCompare t) => GCompare (NF ctx t) where
  (Term a) `gcompare` (Term b) = a `gcompare` b
  (NFVar a) `gcompare` (NFVar b) = a `gcompare` b
  (Term _) `gcompare` (NFVar _) = GLT
  (NFVar _) `gcompare` (Term _) = GGT

data DGNFNonTerminal m ctx t a where
  DGNFNonTerminal :: DM.DMap (NF ctx t) (DGNFProd m a) -> Maybe a -> DGNFNonTerminal m ctx t a

shift :: DGNFNonTerminal m (c ': ctx) t a -> DGNFNonTerminal m ctx t a
shift (DGNFNonTerminal mp eps) = DGNFNonTerminal (DM.mapKeysMonotonic shiftNF mp) eps
  where
    shiftNF :: NF (c ': ctx) t a -> NF ctx t a
    shiftNF (Term x) = Term x
    shiftNF (NFVar IndexZ) = error "unreachable! shift"
    shiftNF (NFVar (IndexS x)) = NFVar x

-- Merge two nonterminals (assuming not both have epsilon)
instance (GCompare t) => Semigroup (DGNFNonTerminal m ctx t a) where
  DGNFNonTerminal m1 e1 <> DGNFNonTerminal m2 e2 = case (e1, e2) of
    (Just _, Just _) -> error "tried to merge two nonterminals with epsilon"
    _ -> DGNFNonTerminal (m1 <> m2) (e1 <|> e2)

-- Is this instance needed?
-- instance Functor (DGNFNTSeq m t) where
--   fmap f (Nil v) = Nil (f v)
--   fmap f (Cons n ns g) = Cons n ns ((f .) . g)

-- awkward to write a Functor instance
fmapDGNFProd :: (a -> d) -> DGNFProd m a b -> DGNFProd m d b
fmapDGNFProd f (DGNFProd ns g) = DGNFProd ns ((f .) . g)

--
instance Functor (DGNFNonTerminal m ctx t) where
  fmap f (DGNFNonTerminal prods null) = DGNFNonTerminal (DM.map (fmapDGNFProd f) prods) (f <$> null)

epsNonTerminal :: a -> DGNFNonTerminal m ctx t a
epsNonTerminal a = DGNFNonTerminal DM.empty (Just a)

tokenNonTerminal :: t a -> DGNFNonTerminal m ctx t a
tokenNonTerminal tok = DGNFNonTerminal (DM.singleton (Term tok) prod) Nothing
  where
    prod :: DGNFProd m a a
    prod = DGNFProd (Nil ()) const

varNonTerminal :: Index ctx a -> DGNFNonTerminal m ctx t a
varNonTerminal idx = DGNFNonTerminal (DM.singleton (NFVar idx) prod) Nothing
  where
    prod :: DGNFProd m a a
    prod = DGNFProd (Nil ()) const

data DGNFGrammar m ctx t a = DGNFGrammar
  { _start :: NonTerminalId m a,
    _nonterminals :: DM.DMap (NonTerminalId m) (DGNFNonTerminal m ctx t)
  }

normalize :: (GCompare t, PrimMonad m) => Grammar '[] t a d -> m (DGNFGrammar m '[] t a)
normalize = normalize'

normalize' :: forall t m ctx a d. (GCompare t, PrimMonad m) => Grammar ctx t a d -> m (DGNFGrammar m ctx t a)
normalize' (gr, _) =
  newTag >>= \n -> case gr of
    Eps a -> return $ DGNFGrammar n (DM.singleton n (epsNonTerminal a))
    Tok t -> return $ DGNFGrammar n (DM.singleton n (tokenNonTerminal t))
    Bot -> return $ DGNFGrammar n DM.empty
    Seq a b -> do
      DGNFGrammar n1 g1 <- normalize' a
      DGNFGrammar n2 g2 <- normalize' b
      let DGNFNonTerminal mp _ = fromMaybe (error "seq") (DM.lookup n1 g1) -- g1 ! n1
          -- TODO: we can guarantee that n1 doesn't have any epsilon?
          n2seq = Cons n2 (Nil ()) const
          nmp = DM.map (`append` n2seq) mp
          nNonTerm = DGNFNonTerminal nmp Nothing
      return $ DGNFGrammar n (DM.unions [DM.singleton n nNonTerm, g1, g2])
    Alt a b -> do
      DGNFGrammar n1 g1 <- normalize' a
      DGNFGrammar n2 g2 <- normalize' b
      let aa = fromMaybe (error "alt 1") (DM.lookup n1 g1)
      let bb = fromMaybe (error "alt 2") (DM.lookup n2 g2)
      return $ DGNFGrammar n (DM.unions [DM.singleton n (aa <> bb), g1, g2])
    Fix g -> do
      DGNFGrammar n' g' <- normalize' g
      -- Type 1
      let originalProds@(DGNFNonTerminal originalProdsMap _) =
            shift $ fromMaybe (error "AAA") (DM.lookup n' g')
          -- shift (g' ! n')
          -- Type 2 and Type 3
          f :: NF (a : ctx) t s -> DGNFProd m v s -> DM.DMap (NF ctx t) (DGNFProd m v)
          f (Term t) p = DM.singleton (Term t) p
          f (NFVar IndexZ) (DGNFProd ns f1) = DM.map (\prod -> mapDGNFProd (uncurry f1) (append prod ns)) originalProdsMap
          f (NFVar (IndexS i)) p = DM.singleton (NFVar i) p
          fn :: forall v. DGNFNonTerminal m (a ': ctx) t v -> DGNFNonTerminal m ctx t v
          fn (DGNFNonTerminal mp eps) = DGNFNonTerminal (mapKeysWith' f mp) eps
          types2And3 :: DM.DMap (NonTerminalId m) (DGNFNonTerminal m ctx t)
          types2And3 = DM.map fn g'
      return $ DGNFGrammar n (DM.unions [DM.singleton n originalProds, types2And3])
    Map f x -> do
      DGNFGrammar n' x' <- normalize' x
      let xx = fromMaybe (error "AAA") (DM.lookup n' x')
      return $ DGNFGrammar n (DM.insert n (f <$> xx) x')
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

appendNTSeq :: DGNFNTSeq m a -> DGNFNTSeq m b -> DGNFNTSeq m (a, b)
appendNTSeq (Nil a) (Nil b) = Nil (a, b)
appendNTSeq (Cons a ns f) n =
  let ns' = appendNTSeq ns n
   in Cons a ns' (\d (c, b) -> (f d c, b))
appendNTSeq (Nil a) (Cons b ns f) = Cons b ns (\d c -> (a, f d c))

-- Given: c -> c1 -> a
-- c -> (c1, b) -> (a, b)
append :: DGNFProd m a c -> DGNFNTSeq m b -> DGNFProd m (a, b) c
append (DGNFProd ns1 f) ns2 = DGNFProd (appendNTSeq ns1 ns2) (\c (d, b) -> (f c d, b))

mapKeysWith' :: (GCompare k2) => (forall v. k1 v -> f v -> DM.DMap k2 f) -> DM.DMap k1 f -> DM.DMap k2 f
mapKeysWith' f mp = DM.unions (map (\(k :=> v) -> f k v) (DM.assocs mp))
