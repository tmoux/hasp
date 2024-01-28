{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Hasp.Ctx where

import Data.Kind (Type)
import Unsafe.Coerce (unsafeCoerce)

data Ctx :: [Type] -> Type where
  CtxZ :: Ctx '[]
  CtxS :: Ctx ctx -> Ctx (a ': ctx)

data Index :: [Type] -> Type -> Type where
  IndexZ :: Index (a ': ctx) a
  IndexS :: Index ctx a -> Index (b ': ctx) a

deriving instance Show (Index ctx a)

len :: Ctx n -> Int
len CtxZ = 0
len (CtxS ctx) = 1 + len ctx

-- See https://homepages.inf.ed.ac.uk/slindley/papers/unembedding.pdf
tshift' :: Int -> Ctx j -> Ctx (a ': i) -> Index j a
tshift' _ CtxZ _ = error "impossible"
tshift' 0 (CtxS _) (CtxS _) = unsafeCoerce IndexZ
tshift' n (CtxS c1) c2 = IndexS (tshift' (n - 1) c1 c2)

tshift :: Ctx j -> Ctx (a ': i) -> Index j a
tshift c1 c2 = tshift' (len c1 - len c2) c1 c2

-- I had to add a f parameter, so Hlist f [a, b, c] represents list of types [f a, f b, f c]

data HList :: (Type -> Type) -> [Type] -> Type where
  HNil :: HList f '[]
  HCons :: f a -> HList f xs -> HList f (a ': xs)

hlookup :: Index ctx a -> HList f ctx -> f a
hlookup IndexZ (HCons x _) = x
hlookup (IndexS v) (HCons _ xs) = hlookup v xs

hmap :: (forall a. f a -> f a) -> HList f ctx -> HList f ctx
hmap _ HNil = HNil
hmap f (HCons x xs) = HCons (f x) (hmap f xs)

-- This stuff below isn't used

data Nat = Zero | Succ Nat

type family CtxLength a :: Nat where
  CtxLength '[] = 'Zero
  CtxLength (_ ': xs) = 'Succ (CtxLength xs)

data Vec :: Type -> Nat -> Type where
  VNil :: Vec a 'Zero
  VCons :: a -> Vec a n -> Vec a ('Succ n)

data Fin :: Nat -> Type where
  FinZ :: Fin ('Succ a)
  FinS :: Fin a -> Fin ('Succ a)

vlookup :: Fin n -> Vec a n -> a
vlookup FinZ (VCons x _) = x
vlookup (FinS n) (VCons _ xs) = vlookup n xs

homl :: (CtxLength ctx ~ n) => Index ctx b -> Vec a n -> a
homl IndexZ (VCons x _) = x
homl (IndexS i) (VCons _ xs) = homl i xs
