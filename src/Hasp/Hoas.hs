{-# HLINT ignore "Redundant lambda" #-}
{-# LANGUAGE DataKinds #-}
{-# HLINT ignore "Use const" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Hasp.Hoas where

-- HOAS to first-order conversion

import Control.Applicative
import Hasp.Ctx (Ctx (..), tshift)
import Hasp.Grammar (Grammar, Grammar' (..))
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

newtype Hoas t a = H {unH :: forall ctx. Ctx ctx -> Grammar ctx a t ()}

-- These instances lets us write parser combinators in a more familiar manner, using <|>, <*>, and friends.
instance Functor (Hoas t) where
  fmap = map

instance Applicative (Hoas t) where
  pure = eps
  f <*> x = map (uncurry ($)) $ seq f x

instance Alternative (Hoas t) where
  empty = bot
  (<|>) = alt

-- Hoas is not a monad, as we need to be able to statically determine whether the grammar is ambiguous.
-- Thus we can't implement monadic bind, where the second parser depends on the output of the first.
-- See https://stackoverflow.com/questions/7861903/

mkG :: (forall ctx. Ctx ctx -> Grammar' ctx a t ()) -> Hoas t a
mkG v = H $ \i -> (v i, ())

eps :: a -> Hoas t a
eps a = mkG $ \_ -> Eps a

seq :: Hoas t a -> Hoas t b -> Hoas t (a, b)
seq (H a) (H b) = mkG $ \i -> Seq (a i) (b i)

tok :: t a -> Hoas t a
tok c = mkG $ \_ -> Tok c

bot :: Hoas t a
bot = mkG $ \_ -> Bot

alt :: Hoas t a -> Hoas t a -> Hoas t a
alt (H a) (H b) = mkG $ \i -> Alt (a i) (b i)

map :: (a -> b) -> Hoas t a -> Hoas t b
map f (H a) = mkG $ \i -> Map f (a i)

fix :: (Hoas t a -> Hoas t a) -> Hoas t a
fix f = mkG $ \i ->
  let v :: Ctx ctx -> Grammar ctx a t ()
      v = \j -> (Var (tshift j (CtxS i)), ())
   in Fix $ unH (f (H v)) (CtxS i)

toTerm :: Hoas t a -> Grammar '[] a t ()
toTerm t = unH t CtxZ

(<|>) :: Hoas t a -> Hoas t a -> Hoas t a
(<|>) = alt

infixl 1 <|>
