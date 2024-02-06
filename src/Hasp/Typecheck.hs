{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Hasp.Typecheck where

import Control.Monad (unless)
import Control.Monad.Except
import qualified Data.Bifunctor as Bifunctor
import Data.Functor.Const
import Data.GADT.Compare
import Data.GADT.Show
import Data.Some
import Hasp.Ctx (HList (..), hlookup, hmap)
import Hasp.Grammar
import Hasp.Types
import Text.Printf (printf)

makeAllGuarded :: (Ord t) => HList (Const (Tp t)) ctx -> HList (Const (Tp t)) ctx
makeAllGuarded = hmap (Bifunctor.first makeGuarded)

type Err = String

type TCMonad = Except Err

check :: Bool -> Err -> TCMonad ()
check b err = unless b $ throwError err

typeof :: (GShow t, GCompare t) => HList (Const (Tp (Some t))) ctx -> Grammar ctx t a d -> TCMonad (Grammar ctx t a (Tp (Some t)))
typeof env (grammar, _) = case grammar of
  Eps a -> return (Eps a, tEps)
  Seq a b -> do
    a'@(_, ta) <- typeof env a
    b'@(_, tb) <- typeof (makeAllGuarded env) b
    check (separable ta tb) (printf "Not separable: %s %s" (show ta) (show tb))
    return (Seq a' b', tConcat ta tb)
  Tok c -> return (Tok c, tChar (mkSome c))
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

typecheck :: (GShow t, GCompare t) => Grammar '[] t a d -> TCMonad (Grammar '[] t a (Tp (Some t)))
typecheck = typeof HNil
