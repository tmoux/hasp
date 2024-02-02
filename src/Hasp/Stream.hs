{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

module Hasp.Stream where

import Data.Data ((:~:) (..))
import Data.GADT.Compare
import Data.GADT.Show
import Data.Some

data Token t a = Token
  { tag :: t a,
    dat :: a
  }

getTag :: Some (Token t) -> Some t
getTag = mapSome tag

-- TODO: do we need fundep?
class Stream s t | s -> t where
  uncons :: s -> Maybe (Some (Token t), s)

-- A tag type for type c, where each value of c has its own tag.
data Tag c a where
  Tag :: c -> Tag c c

deriving instance (Show c) => Show (Tag c a)

deriving instance (Eq c) => Eq (Tag c a)

deriving instance (Ord c) => Ord (Tag c a)

instance (Show c) => GShow (Tag c) where
  gshowsPrec = defaultGshowsPrec

instance (Eq c) => GEq (Tag c) where
  (Tag a) `geq` (Tag b)
    | a == b = Just Refl
    | otherwise = Nothing

instance (Ord c) => GCompare (Tag c) where
  (Tag a) `gcompare` (Tag b)
    | a == b = GEQ
    | a < b = GLT
    | otherwise = GGT

instance Stream [c] (Tag c) where
  uncons [] = Nothing
  uncons (x : xs) = Just (mkSome $ Token (Tag x) x, xs)

unconsList :: (Stream [c] (Tag c)) => [c] -> Maybe (c, [c])
unconsList s =
  uncons s >>= \(tok, rest) ->
    withSome tok $ \case
      Token (Tag _) d -> Just (d, rest)
