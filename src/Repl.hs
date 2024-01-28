module Repl where

import Data.Maybe
import Hasp.Hoas
import Hasp.Parser ( typecheck )
import Hasp.Types

t1 :: Tp
t1 = snd $ fromJust . typecheck . toTerm $ letter
t2 :: Tp
t2 = snd $ fromJust . typecheck . toTerm $ star (paren (chr 'c'))
