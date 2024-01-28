{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Hasp.Grammar where

import Data.Kind (Type)
import Hasp.Ctx

-- TODO: generalize Char type to arbitrary token
data Grammar' :: [Type] -> Type -> Type -> Type where
  Eps :: a -> Grammar' ctx a d
  Seq :: Grammar ctx a d -> Grammar ctx b d -> Grammar' ctx (a, b) d
  Chr :: Char -> Grammar' ctx Char d
  Bot :: Grammar' ctx a d
  Alt :: Grammar ctx a d -> Grammar ctx a d -> Grammar' ctx a d
  Map :: (a -> b) -> Grammar ctx a d -> Grammar' ctx b d
  Fix :: Grammar (a ': ctx) a d -> Grammar' ctx a d
  Var :: Index ctx a -> Grammar' ctx a d

type Grammar ctx a d = (Grammar' ctx a d, d)
