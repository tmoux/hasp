{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}

module Hasp.Parser (toParser, Parser (..)) where

import Control.Monad.Except
import qualified Data.Set as S
import Hasp.Ctx
import Hasp.Grammar
import Hasp.Types
import Prelude hiding (null, seq)

-- TODO: change Maybe to a more informative error type
newtype Parser a = P {unP :: [Char] -> Maybe (a, [Char])}
  deriving (Functor)

instance Applicative Parser where
  pure a = P (\s -> pure (a, s))
  (<*>) = ap

instance Monad Parser where
  (P p) >>= f = P (p >=> (\(v, s') -> unP (f v) s'))

eps :: a -> Parser a
eps = pure

seq :: Parser a -> Parser b -> Parser (a, b)
seq p1 p2 = (,) <$> p1 <*> p2

chr :: Char -> Parser Char
chr c =
  P
    ( \case
        x : xs | x == c -> return (c, xs)
        _ -> Nothing
    )

bot :: Parser a
bot = P (const Nothing)

alt :: Tp -> Parser a -> Tp -> Parser a -> Parser a
alt t1 p1 t2 p2 =
  P
    ( \s ->
        let r1 = unP p1 s
            r2 = unP p2 s
         in case s of
              [] | t1.null -> r1
              [] | t2.null -> r2
              [] | otherwise -> Nothing
              c : _ | S.member c t1.first -> r1
              c : _ | S.member c t2.first -> r2
              _ : _ | t1.null -> r1
              _ : _ | t2.null -> r2
              _ : _ | otherwise -> Nothing
    )

parse :: Grammar ctx a Tp -> HList Parser ctx -> Parser a
parse o@(gr, _) env = case gr of
  (Eps v) -> eps v
  (Seq g1 g2) -> seq p1 p2
    where
      p1 = parse g1 env
      p2 = parse g2 env
  (Chr c) -> chr c
  Bot -> bot
  (Alt g1 g2) -> alt (snd g1) p1 (snd g2) p2
    where
      p1 = parse g1 env
      p2 = parse g2 env
  (Map f x) -> f <$> parse x env
  (Fix g) -> parse g (HCons p env)
    where
      p = parse o env
  (Var v) -> hlookup v env

toParser :: Grammar '[] a Tp -> Parser a
toParser g = parse g HNil
