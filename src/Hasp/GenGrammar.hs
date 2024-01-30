{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Hasp.GenGrammar where

import Data.Kind (Type)
import Hasp.Ctx

data Grammar' :: [Type] -> Type -> Type -> Type -> Type where
  Eps :: a -> Grammar' ctx a t d
  Seq :: Grammar ctx a t d -> Grammar ctx b t d -> Grammar' ctx (a, b) t d
  Chr :: t -> Grammar' ctx t t d
  Bot :: Grammar' ctx a t d
  Alt :: Grammar ctx a t d -> Grammar ctx a t d -> Grammar' ctx a t d
  Map :: (a -> b) -> Grammar ctx a t d -> Grammar' ctx b t d
  Fix :: Grammar (a ': ctx) a t d -> Grammar' ctx a t d
  Var :: Index ctx a -> Grammar' ctx a t d

type Grammar ctx a t d = (Grammar' ctx a t d, d)
