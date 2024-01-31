{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Hasp.TH (Checked, checkParser) where

import Control.Monad.Except (runExcept)
import Hasp.Hoas
import Hasp.Typecheck
import Language.Haskell.TH

-- Don't export constructor
data Checked a = Checked

checkParser :: forall t a. (Show t, Ord t) => Hoas t a -> Code Q (Checked (Hoas t a))
checkParser p = case runExcept (typecheck (toTerm p)) of
  Left err -> error err
  Right _ -> [||Checked||]

-- makeParser p = case runExcept (toParser <$> typecheck (toTerm p) :: Except Err (Parser s a)) of
--   Left err -> error err
--   Right parser -> [||parser||]
