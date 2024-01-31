{-# LANGUAGE TemplateHaskell #-}

module CheckParsers where

import Hasp.Hoas
import Hasp.TH
import Parsers

checkLetter :: Checked (Hoas Char Char)
checkLetter = $$(checkParser letter)

checkBadFixpoint :: Checked (Hoas Char Char)
checkBadFixpoint = $$(checkParser hBadFixpoint)
