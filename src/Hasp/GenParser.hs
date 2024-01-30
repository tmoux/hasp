{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Hasp.GenParser (toParser, Parser (..), parse) where

import Control.Monad.Except
import qualified Data.Set as S
import Hasp.Ctx
import Hasp.Grammar
import Hasp.Types
import Text.Parsec (Stream, uncons)
import Prelude hiding (null, seq)

newtype ParserT s m a = P' {unP' :: s -> m (Maybe (a, s))}
  deriving (Functor)

parse :: (Stream s m t) => ParserT s m a -> s -> m (Maybe (a, s))
parse = unP'

-- TODO: change Maybe to a more informative error type
newtype Parser a = P {unP :: [Char] -> Maybe (a, [Char])}
  deriving (Functor)

instance (Monad m) => Applicative (ParserT s m) where
  pure a = P' (\s -> pure $ pure (a, s))
  (<*>) = ap

instance (Monad m) => Monad (ParserT s m) where
  (P' p) >>= f =
    P'
      ( p
          >=> ( \case
                  Nothing -> return Nothing
                  Just (a, s') -> unP' (f a) s'
              )
      )

instance Applicative Parser where
  pure a = P (\s -> pure (a, s))
  (<*>) = ap

instance Monad Parser where
  (P p) >>= f = P (p >=> (\(v, s') -> unP (f v) s'))

eps :: a -> ParserT s m a
eps = pure

seq :: ParserT s m a -> ParserT s m b -> ParserT s m (a, b)
seq p1 p2 = (,) <$> p1 <*> p2

chr :: (Stream s m t) => t -> ParserT s m t
chr c =
  P'
    ( \s -> do
        uncons s >>= \case
          Just (t, rest) | t == c -> return $ return (c, rest)
          _ -> return Nothing
    )

bot :: ParserT s m a
bot = P' (const $ return Nothing)

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

toParser' :: Grammar ctx a Tp -> HList Parser ctx -> Parser a
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

toParser :: Grammar '[] a Tp -> Parser a
toParser g = toParser' g HNil
