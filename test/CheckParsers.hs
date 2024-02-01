{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module CheckParsers where

import Hasp.Hoas
import Hasp.TH
import Parsers

checkLetter :: Checked (Hoas Char Char)
checkLetter = $$(checkParser letter)

-- These declarations do not compile:
-- checkBadFixpoint :: Checked (Hoas Char Char)
-- checkBadFixpoint = $$(checkParser hBadFixpoint)

-- checkBadDisj :: Checked (Hoas Char Char)
-- checkBadDisj = $$(checkParser hBadDisj)
