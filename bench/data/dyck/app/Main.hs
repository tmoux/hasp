{-# LANGUAGE DeriveGeneric #-}

module Main where

import GHC.Generics (Generic)
import System.Environment (getArgs)
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic (genericArbitrary)

newtype Dyck = Dyck [Dyck]
  deriving (Generic)

instance Show Dyck where
  show (Dyck xs) = concatMap (\x -> "(" ++ show x ++ ")") xs

instance Arbitrary Dyck where
  arbitrary = genericArbitrary
  shrink = genericShrink

main :: IO ()
main = do
  size : len : _ <- (fmap . fmap) read getArgs
  let
    go = do
      gen <- show <$> (generate (resize size arbitrary) :: IO Dyck)
      if len <= length gen && length gen <= len + 1000 then return gen else go
  gen <- go
  putStrLn gen
