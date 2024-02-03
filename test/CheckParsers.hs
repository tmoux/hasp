{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module CheckParsers where

import Hasp.Char
import Hasp.Hoas
import Hasp.TH
import Hasp.Examples.Parsers

checkLetter :: Checked (Hoas CharTag Char)
checkLetter = $$(checkParser letter)

-- These declarations do not compile:
-- checkBadFixpoint :: Checked (Hoas CharTag Char)
-- checkBadFixpoint = $$(checkParser hBadFixpoint)

-- checkBadDisj :: Checked (Hoas CharTag Char)
-- checkBadDisj = $$(checkParser hBadDisj)
