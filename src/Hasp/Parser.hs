{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Hasp.Parser where

import Control.Monad.Except
import Data.Functor.Const
import Data.Kind (Type)
import qualified Data.Set as S
import Hasp.Ctx
import Hasp.Types
import Prelude hiding (null, seq)
import Text.Printf

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

makeAllGuarded :: HList (Const Tp) ctx -> HList (Const Tp) ctx
makeAllGuarded = hmap (\(Const x) -> Const $ makeGuarded x)

type Err = String

type TCMonad = Except String

check :: Bool -> String -> TCMonad ()
check b err = unless b $ throwError err

typeof :: HList (Const Tp) ctx -> Grammar ctx a d -> TCMonad (Grammar ctx a Tp)
typeof env (grammar, _) = case grammar of
  Eps a -> return (Eps a, tEps)
  Seq a b -> do
    a'@(_, ta) <- typeof env a
    b'@(_, tb) <- typeof (makeAllGuarded env) b
    check (separable ta tb) (printf "Not separable: %s %s" (show ta) (show tb))
    return (Seq a' b', tConcat ta tb)
  Chr c -> return (Chr c, tChar c)
  Bot -> return (Bot, tBot)
  Alt a b -> do
    a'@(_, ta) <- typeof env a
    b'@(_, tb) <- typeof env b
    check (apart ta tb) (printf "Not apart: %s %s" (show ta) (show tb))
    return (Alt a' b', tDisj ta tb)
  Map f a -> do
    a'@(_, t) <- typeof env a
    return (Map f a', t)
  Fix g -> do
    t <- fixpoint (\tp -> snd <$> typeof (HCons (Const tp) env) g)
    check t.guarded (printf "Not guarded: %s" (show t))
    g'@(_, t') <- typeof (HCons (Const t) env) g
    return (Fix g', t')
  Var x -> do
    let (Const t) = hlookup x env
    -- Note: don't check if variable is guarded here.
    return (Var x, t)

typecheck :: Grammar '[] a d -> TCMonad (Grammar '[] a Tp)
typecheck = typeof HNil

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

-- I had to add a f parameter, so Hlist f [a, b, c] represents list of types [f a, f b, f c]

data HList :: (Type -> Type) -> [Type] -> Type where
  HNil :: HList f '[]
  HCons :: f a -> HList f xs -> HList f (a ': xs)

hlookup :: Index ctx a -> HList f ctx -> f a
hlookup IndexZ (HCons x _) = x
hlookup (IndexS v) (HCons _ xs) = hlookup v xs

hmap :: (forall a. f a -> f a) -> HList f ctx -> HList f ctx
hmap _ HNil = HNil
hmap f (HCons x xs) = HCons (f x) (hmap f xs)

-- Env should be a list of parsers
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
      p = parse o env -- ????
  (Var v) -> hlookup v env

toParser :: Grammar '[] a Tp -> Parser a
toParser g = parse g HNil
