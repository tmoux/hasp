{-# HLINT ignore "Redundant lambda" #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# HLINT ignore "Use const" #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Hasp.Hoas where

-- HOAS to first-order conversion

import Control.Applicative
import Control.Monad.Except hiding (fix)
import Hasp.Ctx (Ctx (..), tshift)
import Hasp.Grammar (Grammar, Grammar' (..))
import Text.Parsec (Stream (..))
import Prelude hiding (any, map, seq)

{-
  Idea: express combinators as a typeclass
  Uses ideas from here: https://homepages.inf.ed.ac.uk/slindley/papers/unembedding.pdf
-}

-- TODO: find names that don't clash with prelude
-- Or just import it qualified
-- chr doesn't need to be a char, can just be a unit or something
-- class ParsingGrammar e where
--   eps :: a -> e a
--   seq :: e a -> e b -> e (a, b)
--   chr :: Char -> e Char
--   bot :: e a
--   alt :: e a -> e a -> e a
--   map :: (a -> b) -> e a -> e b
--   fix :: (e a -> e a) -> e a

-- instance (ParsingGrammar e) => Functor e where
--   fmap = e
-- newtype Hoas' s m a = H {unH :: forall ctx. Ctx ctx -> Grammar ctx s m a ()}

newtype Hoas a = H {unH :: forall ctx. Ctx ctx -> Grammar ctx a ()}

-- These instances lets us write parser combinators in a more familiar manner, using <|>, <*>, and friends.
instance Functor Hoas where
  fmap = map

instance Applicative Hoas where
  pure = eps
  f <*> x = map (uncurry ($)) $ seq f x

instance Alternative Hoas where
  empty = bot
  (<|>) = alt

-- Hoas is not a monad, as we need to be able to statically determine whether the grammar is ambiguous.
-- Thus we can't implement monadic bind, where the second parser depends on the output of the first.
-- See https://stackoverflow.com/questions/7861903/

mkG :: (forall ctx. Ctx ctx -> Grammar' ctx a ()) -> Hoas a
mkG v = H $ \i -> (v i, ())

eps :: a -> Hoas a
eps a = mkG $ \_ -> Eps a

seq :: Hoas a -> Hoas b -> Hoas (a, b)
seq (H a) (H b) = mkG $ \i -> Seq (a i) (b i)

char :: Char -> Hoas Char
char c = mkG $ \_ -> Chr c

bot :: Hoas a
bot = mkG $ \_ -> Bot

alt :: Hoas a -> Hoas a -> Hoas a
alt (H a) (H b) = mkG $ \i -> Alt (a i) (b i)

map :: (a -> b) -> Hoas a -> Hoas b
map f (H a) = mkG $ \i -> Map f (a i)

fix :: (Hoas a -> Hoas a) -> Hoas a
fix f = mkG $ \i ->
  let v :: Ctx ctx -> Grammar ctx a ()
      v = \j -> (Var (tshift j (CtxS i)), ())
   in Fix $ unH (f (H v)) (CtxS i)

toTerm :: Hoas a -> Grammar '[] a ()
toTerm t = unH t CtxZ
