{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Hasp.Parser where

import Control.Monad (ap, (>=>))
import Data.GADT.Compare
import qualified Data.Set as S
import Data.Some
import Data.Type.Equality
import Hasp.Ctx
import Hasp.Grammar
import Hasp.Stream
import Hasp.Types
import Prelude hiding (null, seq)

newtype Parser s a = P {unP :: s -> Maybe (a, s)}
  deriving (Functor)

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

-- TODO: rename this to tok
chr :: (GEq t, Stream s t) => t a -> Parser s a
chr c =
  P
    ( \s ->
        case uncons s of
          Just (someTok, rest) -> withSome someTok $ \(Token t a) -> case c `geq` t of
            Just Refl -> Just (a, rest)
            Nothing -> Nothing
          Nothing -> Nothing
    )

bot :: Parser s a
bot = P (const Nothing)

-- TODO: Don't use set membership checks here. Instead, generate the code that cases on all the chars in t1/t2.
alt :: (Stream s t, GCompare t) => Tp (Some t) -> Parser s a -> Tp (Some t) -> Parser s a -> Parser s a
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
                | S.member (getTag c) t1.first -> r1
                | S.member (getTag c) t2.first -> r2
                | t1.null -> r1
                | t2.null -> r2
                | otherwise -> Nothing
    )

toParser' :: (Stream s t, GEq t, GCompare t) => Grammar ctx t a (Tp (Some t)) -> HList (Parser s) ctx -> Parser s a
toParser' o@(gr, _) env = case gr of
  (Eps v) -> eps v
  (Seq g1 g2) -> seq p1 p2
   where
    p1 = toParser' g1 env
    p2 = toParser' g2 env
  (Tok c) -> chr c
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

toParser :: (Stream s t, GEq t, GCompare t) => Grammar '[] t a (Tp (Some t)) -> Parser s a
toParser g = toParser' g HNil
