{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Hasp.Grammar where

import Data.Kind (Type)
import Hasp.Ctx

data Grammar' :: [Type] -> (Type -> Type) -> Type -> Type -> Type where
  Eps :: a -> Grammar' ctx t a d
  Seq :: Grammar ctx t a d -> Grammar ctx t b d -> Grammar' ctx t (a, b) d
  Tok :: t a -> Grammar' ctx t a d
  Bot :: Grammar' ctx t a d
  Alt :: Grammar ctx t a d -> Grammar ctx t a d -> Grammar' ctx t a d
  Map :: (a -> b) -> Grammar ctx t a d -> Grammar' ctx t b d
  Fix :: Grammar (a ': ctx) t a d -> Grammar' ctx t a d
  Var :: Index ctx a -> Grammar' ctx t a d

type Grammar ctx t a d = (Grammar' ctx t a d, d)
