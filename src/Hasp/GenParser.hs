{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Hasp.GenParser where

import Control.Monad.Except
import Data.Functor.Identity (Identity (..))
import qualified Data.Set as S
import Hasp.Ctx
import Hasp.GenGrammar
import Hasp.GenTypes
import qualified Text.Parsec as Parsec
import Prelude hiding (null, seq)

newtype Parser s a = P {unP :: s -> Maybe (a, s)}
  deriving (Functor)

type Stream s t = Parsec.Stream s Identity t

uncons :: (Stream s t) => s -> Maybe (t, s)
uncons = runIdentity . Parsec.uncons

parse :: (Stream s t) => Parser s a -> s -> Maybe (a, s)
parse = unP

instance Applicative (Parser s) where
  pure a = P (\s -> pure (a, s))
  (<*>) = ap

instance Monad (Parser s) where
  (P p) >>= f = P (p >=> (\(v, s') -> unP (f v) s'))

eps :: a -> Parser s a
eps = pure

seq :: Parser s a -> Parser s b -> Parser s (a, b)
seq p1 p2 = (,) <$> p1 <*> p2

chr :: (Eq t, Stream s t) => t -> Parser s t
chr c =
  P
    ( \s ->
        case uncons s of
          Just (t, rest) | t == c -> return (c, rest)
          _ -> Nothing
    )

bot :: Parser s a
bot = P (const Nothing)

alt :: (Stream s t, Ord t) => Tp t -> Parser s a -> Tp t -> Parser s a -> Parser s a
alt t1 p1 t2 p2 =
  P
    ( \s ->
        let r1 = unP p1 s
            r2 = unP p2 s
         in case uncons s of
              Nothing | t1.null -> r1
              Nothing | t2.null -> r2
              Nothing | otherwise -> Nothing
              Just (c, _)
                | S.member c t1.first -> r1
                | S.member c t2.first -> r2
                | t1.null -> r1
                | t2.null -> r2
                | otherwise -> Nothing
    )

toParser' :: (Stream s t, Ord t) => Grammar ctx a t (Tp t) -> HList (Parser s) ctx -> Parser s a
toParser' o@(gr, _) env = case gr of
  (Eps v) -> eps v
  (Seq g1 g2) -> seq p1 p2
    where
      p1 = toParser' g1 env
      p2 = toParser' g2 env
  (Chr c) -> chr c
  Bot -> bot
  (Alt g1 g2) -> alt (snd g1) p1 (snd g2) p2
    where
      p1 = toParser' g1 env
      p2 = toParser' g2 env
  (Map f x) -> f <$> toParser' x env
  (Fix g) -> toParser' g (HCons p env)
    where
      p = toParser' o env
  (Var v) -> hlookup v env

toParser :: (Stream s t, Ord t) => Grammar '[] a t (Tp t) -> Parser s a
toParser g = toParser' g HNil
