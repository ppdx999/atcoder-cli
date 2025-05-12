{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Provider.AtcoderSpec qualified
import Provider.ConfigSpec qualified
import Provider.LanguageSpec qualified
import Provider.SessionSpec qualified
import Test.Hspec
import TypesSpec qualified
import Usecase.DownloadSpec qualified
import Usecase.InitSpec qualified
import Usecase.LoginSpec qualified
import Usecase.SubmitSpec qualified
import Usecase.TestSpec qualified

main :: IO ()
main = hspec $ do
  describe "Types" TypesSpec.spec
  describe "Usecase.Init" Usecase.InitSpec.spec
  describe "Usecase.Download" Usecase.DownloadSpec.spec
  describe "Usecase.Login" Usecase.LoginSpec.spec
  describe "Usecase.Test" Usecase.TestSpec.spec
  describe "Usecase.Submit" Usecase.SubmitSpec.spec
  describe "Provider.Atcoder" Provider.AtcoderSpec.spec
  describe "Provider.Session" Provider.SessionSpec.spec
  describe "Provider.Config" Provider.ConfigSpec.spec
  describe "Provider.Language" Provider.LanguageSpec.spec
