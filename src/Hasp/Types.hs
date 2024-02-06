{-# LANGUAGE OverloadedRecordDot #-}

module Hasp.Types where

import qualified Data.Set as S
import Prelude hiding (null)

data Tp t = Tp
  { null :: Bool
  , first :: S.Set t
  , flast :: S.Set t
  , guarded :: Bool
  }
  deriving (Show, Eq)

makeGuarded :: (Ord t) => Tp t -> Tp t
makeGuarded t = t{guarded = True}

-- Separability predicate: is L1 * L2 unambiguous?
separable :: (Ord t) => Tp t -> Tp t -> Bool
separable t1 t2 = S.null (S.intersection t1.flast t2.first) && not t1.null

-- Apartness predicate: is L1 v L2 unambiguous?
apart :: (Ord t) => Tp t -> Tp t -> Bool
apart t1 t2 = S.null (S.intersection t1.first t2.first) && not (t1.null && t2.null)

tBot :: Tp t
tBot = Tp{null = False, first = S.empty, flast = S.empty, guarded = True}

tEps :: Tp t
tEps = Tp{null = True, first = S.empty, flast = S.empty, guarded = True}

tChar :: t -> Tp t
tChar c = Tp{null = False, first = S.singleton c, flast = S.empty, guarded = True}

-- Assumes t1 and t2 are apart
tDisj :: (Ord t) => Tp t -> Tp t -> Tp t
tDisj t1 t2 =
  Tp
    { null = t1.null || t2.null
    , first = S.union t1.first t2.first
    , flast = S.union t1.flast t2.flast
    , guarded = t1.guarded && t2.guarded
    }

ifb :: Bool -> S.Set t -> S.Set t
ifb b s = if b then s else S.empty

-- Assumes t1 and t2 are separable
tConcat :: (Ord t) => Tp t -> Tp t -> Tp t
tConcat t1 t2
  | t1 == tBot = tBot
  | t2 == tBot = tBot
  | otherwise =
      Tp
        { null = t1.null && t2.null
        , first = S.union t1.first (ifb t1.null t2.first)
        , flast = S.union t2.flast (ifb t2.null (S.union t2.first t1.flast))
        , guarded = t1.guarded
        }

tStar :: (Ord t) => Tp t -> Tp t
tStar t =
  Tp
    { null = True
    , first = t.first
    , flast = S.union t.flast t.first
    , guarded = t.guarded
    }

tMin :: Tp t
tMin = Tp{null = False, first = S.empty, flast = S.empty, guarded = False}

fixpoint :: (Monad m, Eq t) => (Tp t -> m (Tp t)) -> m (Tp t)
fixpoint f = go tMin
 where
  go t = do
    t' <- f t
    if t == t'
      then return t
      else go t'
