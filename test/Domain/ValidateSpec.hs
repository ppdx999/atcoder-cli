{-# LANGUAGE OverloadedStrings #-}

module Domain.ValidateSpec (spec) where

import Domain.Types
import Domain.Validate
import Test.Hspec

spec :: Spec
spec = do
  describe "mkContestId" $ do
    it "accepts valid contest ids" $
      mkContestId "abc100" `shouldBe` Right (ContestId "abc100")

    it "rejects empty contest id" $
      mkContestId "" `shouldBe` Left (InvalidContestId "Contest ID cannot be empty.")

    it "rejects invalid characters in contest id" $
      mkContestId "Abc100" `shouldSatisfy` isLeft

  describe "mkProblemId" $ do
    it "accepts valid problem ids" $
      mkProblemId "a" `shouldBe` Right (ProblemId "a")

    it "rejects empty problem id" $
      mkProblemId "" `shouldBe` Left (InvalidProblemId "Problem ID cannot be empty.")

    it "rejects invalid characters in problem id" $
      mkProblemId "A1" `shouldSatisfy` isLeft

  describe "mkLanguageId" $ do
    it "accepts positive language id" $
      mkLanguageId 4001 `shouldBe` Right (LanguageId 4001)

    it "rejects non-positive language id" $
      mkLanguageId 0 `shouldBe` Left (InvalidLanguageId 0)

  describe "mkSourceFile" $ do
    it "accepts non-empty file path" $
      mkSourceFile "Main.hs" `shouldBe` Right (SourceFile "Main.hs")

    it "rejects empty file path" $
      mkSourceFile "" `shouldBe` Left (SourceFileNotFound "")

-- ヘルパー
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
