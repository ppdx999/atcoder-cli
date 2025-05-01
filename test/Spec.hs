{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Test.Hspec
import TypesSpec qualified
import Usecase.InitSpec qualified

main :: IO ()
main = hspec $ do
  describe "Types" TypesSpec.spec
  describe "Usecase.Init" Usecase.InitSpec.spec
