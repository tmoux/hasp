{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Hasp.Typecheck where

import Control.Monad.Except
import Data.Functor.Const
import Hasp.Ctx
import Hasp.Grammar
import Hasp.Types
import Text.Printf (printf)

makeAllGuarded :: HList (Const Tp) ctx -> HList (Const Tp) ctx
makeAllGuarded = hmap (\(Const x) -> Const $ makeGuarded x)

type Err = String

type TCMonad = Except Err

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
