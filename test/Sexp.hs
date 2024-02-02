{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Sexp where

-- Parse s-expressions with alphanumeric atoms

import Test.Tasty.HUnit
import Hasp.Examples.Sexp


unit_sexp1 :: IO ()
unit_sexp1 = countAtoms "(a b c)" @?= Just 3

unit_sexp2 :: IO ()
unit_sexp2 = countAtoms "((a)" @?= Nothing

unit_sexp3 :: IO ()
unit_sexp3 = countAtoms "(a b c (de fg hij klm) zz e)" @?= Just 9

unit_sexp4 :: IO ()
unit_sexp4 = countAtoms "( q w e r ff (d e f ) (((((e)))))  \n\n           )" @?= Just 9

unit_sexp5 :: IO ()
unit_sexp5 = countAtoms "a12345" @?= Just 1