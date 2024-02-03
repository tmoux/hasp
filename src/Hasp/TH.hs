{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Hasp.TH (Checked, checkParser) where

import Control.Monad.Except (runExcept)
import Data.GADT.Compare
import Data.GADT.Show
import Hasp.Hoas
import Hasp.Typecheck
import Language.Haskell.TH

-- Don't export constructor
data Checked a = Checked

checkParser :: forall t a. (GShow t, GCompare t) => Hoas t a -> Code Q (Checked (Hoas t a))
checkParser p = case runExcept $ (typecheck . toTerm) p of
  Left err -> Code $ fail err
  Right _ -> [||Checked||]

-- makeParser p = case runExcept (toParser <$> typecheck (toTerm p) :: Except Err (Parser s a)) of
--   Left err -> error err
--   Right parser -> [||parser||]
