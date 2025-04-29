{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Domain.ValidateSpec qualified
import Test.Hspec

main :: IO ()
main = hspec $ do
  Domain.ValidateSpec.spec
