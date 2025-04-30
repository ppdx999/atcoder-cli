{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import DomainSpec qualified
import Test.Hspec

main :: IO ()
main = hspec $ do
  DomainSpec.spec
