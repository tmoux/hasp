module Hasp.Resolved where

import qualified Data.Dependent.Map as DM
import Data.GADT.Compare
import Data.Kind (Type)
import Data.Some
import Hasp.Parser (Parser (..))
import Hasp.Stream
import Prelude hiding (null)

-- This is the type of resolved DGNF grammar (no variables from fixed points)
-- The intention is to make it as easy as possible to convert into a Parser.

-- For now, we consider grammars parameterized by a token type (ignoring lexing).
-- This token type is a GADT (so we can have a token constructor for ints, a token constructor for unit, etc.)

-- -- Specialize DM.lookup for DMap with a constant value type
-- lookupConst :: (GCompare k) => k v -> DM.DMap k (Const f) -> Maybe f
-- lookupConst key mp = getConst <$> DM.lookup key mp

data NTSeq :: (Type -> Type) -> Type -> Type where
  Nil :: a -> NTSeq t a
  Cons :: NonTerminal t b -> NTSeq t c -> (b -> c -> a) -> NTSeq t a

data Prod :: (Type -> Type) -> Type -> Type -> Type where
  Prod :: NTSeq t c -> (b -> c -> a) -> Prod t a b

data NonTerminal t a = NonTerminal
  { _productions :: DM.DMap t (Prod t a),
    _null :: Maybe a
  }

parserFromNT :: forall s t a. (Stream s t, GCompare t) => NonTerminal t a -> Parser s a
parserFromNT (NonTerminal productions null) =
  P
    ( \s -> case uncons s of
        Just (someTok, rest) -> withSome someTok $
          \(Token t a) -> do
            case DM.lookup t productions of
              Just prod ->
                let prodParser :: Parser s a
                    prodParser = parserFromProd a prod
                 in unP prodParser rest
              Nothing -> null >>= \nullVal -> return (nullVal, s)
        Nothing ->
          null >>= \nullVal -> return (nullVal, s)
    )

parserFromProd :: (Stream s t, GCompare t) => c -> Prod t a c -> Parser s a
parserFromProd tokVal (Prod ntseq f) = f tokVal <$> parserFromNTSeq ntseq

parserFromNTSeq :: (Stream s t, GCompare t) => NTSeq t a -> Parser s a
parserFromNTSeq (Nil v) = pure v
parserFromNTSeq (Cons n ns f) = f <$> parserFromNT n <*> parserFromNTSeq ns
