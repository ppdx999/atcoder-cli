{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import DomainSpec qualified
import Test.Hspec
import Usecase.InitSpec qualified

main :: IO ()
main = hspec $ do
  describe "Domain" DomainSpec.spec
  describe "Usecase.Init" Usecase.InitSpec.spec
