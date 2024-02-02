{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}

module Hasp.Stream where

import Data.Some

data Token t a = Token
  { tag :: t a,
    dat :: a
  }

getTag :: Some (Token t) -> Some t
getTag = mapSome tag

class Stream s t | s -> t where
  uncons :: s -> Maybe (Some (Token t), s)

-- A tag type with a single variant.
data SingleTag t a where
  Trivial :: SingleTag t t

instance Stream [t] (SingleTag t) where
  uncons [] = Nothing
  uncons (x : xs) = Just (mkSome $ Token Trivial x, xs)
