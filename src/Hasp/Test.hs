{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Hasp.Test where

import Data.Dependent.Map
import Data.Dependent.Sum
import Data.GADT.Show (GShow)
import qualified Data.Map.Strict as M
import Data.Some (Some)
import Data.Unique.Tag
import Hasp.Ctx (Index (IndexS, IndexZ))
import Hasp.Examples.Parsers
import Hasp.Examples.Sexp
import Hasp.Grammar (Grammar, Grammar' (..))
import Hasp.Hoas
import qualified Hasp.Hoas as H
import Hasp.Normalization
import Hasp.Types (Tp)
import Data.Kind (Type)
import Control.Monad.ST

data Gadt :: Type -> Type where
  GadtCon :: a -> Gadt a

instance Show a => Show (Gadt a) where
  show (GadtCon a) = show a


main :: ST s (Gadt Bool)
main = do
  x <- newTag
  y <- newTag
  -- z <- newTag
  let m1 = fromList [x :=> GadtCon True, y :=> GadtCon "hello"]
      -- m2 = fromList [x :=> (17 :: Int), z :=> (True, x)]
  -- the type checker would (rightly) reject this line:
  -- m3 = singleton y ("foo", "bar")

  return (m1 ! x)
  -- print (m1 ! x)
  -- print (m1 ! y)
  -- print (m2 ! x)
  -- print (m1 ! snd (m2 ! z))

p :: Hoas TTag ()
-- p = eps 0
-- p = tok LP <* tok RP
-- p = fix (\ss -> eps () <|> tok LP <* ss)
-- p =
--   fix
--     ( \s ->
--         (tok LP <* fix (\ss -> eps () <|> (s <* ss)) <* tok RP)
--           <|> H.map (const ()) (tok Atom)
--     )

-- p = tok LP <* tok RP <* tok LP

p = fix (\s -> tok LP <* (s <|> eps ()) <* tok RP)

-- \ss -> eps <|> s * ss
--- q :: Grammar ((() : ctx) t ((), ()) ()
-- q :: Grammar (a : ctx) t () ()
-- q =
--   ( Fix
--       ( Alt
--           (Eps (), ())
--           ( Map
--               (const ())
--               ( Seq
--                   (Var (IndexS IndexZ), ())
--                   (Var IndexZ, ()),
--                 ()
--               ),
--             ()
--           ),
--         ()
--       ),
--     ()
--   )

-- q :: Grammar (a : b : ctx) t () ()
-- q =
--   ( Alt
--       (Eps (), ())
--       ( Map
--           (const ())
--           ( Seq
--               (Var (IndexS IndexZ), ())
--               (Var IndexZ, ()),
--             ()
--           ),
--         ()
--       ),
--     ()
--   )

{-
g :: Grammar '[] TTag () (Tp (Some TTag))
g = makeTypecheck p

-- d :: M.Map NonTerminalId [NF '[] TTag]
-- d :: M.Map NonTerminalId [NF (a : b : ctx) TTag]
-- d = (unDGNF $ normalize g) 99
d = normalize g

display :: (GShow t) => DGNF ctx t -> String
display (DGNF n m) = "start: " ++ show n ++ "\n" ++ concatMap f (M.toList m)
  where
    f (key, vals) = concatMap (\v -> show key ++ " --> " ++ show v ++ "\n") vals

-- 0 --> eps
-- 1 --> IndexS IndexZ [3]
-- 2 --> IndexS IndexZ []
-- 3 --> IndexZ []
-- 99 --> eps
-- 99 --> IndexS IndexZ [3]

-}
