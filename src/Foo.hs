{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}

module Foo where

import Data.Kind (Type)

data G :: Type -> Type where
  G1 :: a -> G a
  G2 :: G a -> G b -> G (a, b)

class Foo a where
  foo :: a -> Bool

instance (Foo a, Foo b) => Foo (a, b) where
  foo (x, y) = foo x && foo y

-- instance (Foo c) => Foo (G c) where
--   foo (G1 x) = foo x
--   foo (G2 x y) = foo x && foo y


-- test :: (Foo (a, b)) => (a, b) -> Bool
-- test (x, _) = foo x

data Pair a b = Pair a b
  deriving (Show, Eq)

instance (Foo a, Foo b) => Foo (Pair a b) where
  foo (Pair x y) = foo x && foo y

instance {-# OVERLAPS #-} Foo (Pair Int Bool) where
  foo (Pair _ _) = True