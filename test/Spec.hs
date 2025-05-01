{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Provider.AtcoderSpec qualified
import Test.Hspec
import TypesSpec qualified
import Usecase.DownloadSpec qualified
import Usecase.InitSpec qualified

main :: IO ()
main = hspec $ do
  describe "Types" TypesSpec.spec
  describe "Usecase.Init" Usecase.InitSpec.spec
  describe "Usecase.Download" Usecase.DownloadSpec.spec
  describe "Provider.Atcoder" Provider.AtcoderSpec.spec
