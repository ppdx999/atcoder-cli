{-# LANGUAGE OverloadedStrings #-}

module TypesSpec (spec) where

import Data.Either (isLeft, isRight) -- isRight を追加
import Test.Hspec
import Types

spec :: Spec
spec = do
  describe "validateContestId" $ do
    it "accepts valid contest ids" $ do
      let result = validateContestId "abc100"
      result `shouldSatisfy` isRight -- まず Right であることを確認
      result `shouldBe` Right (ContestId "abc100")

    it "rejects empty contest id" $
      -- Left の値は直接比較できる (AppError のコンストラクタはエクスポートされているため)
      validateContestId "" `shouldBe` Left (InvalidContestId "Contest ID cannot be empty.")

    it "rejects invalid characters in contest id" $
      validateContestId "Abc100" `shouldSatisfy` isLeft

  describe "validateProblemId" $ do
    it "accepts valid problem ids" $ do
      let result = validateProblemId "a"
      result `shouldSatisfy` isRight
      result `shouldBe` Right (ProblemId "a")

    it "rejects empty problem id" $
      validateProblemId "" `shouldBe` Left (InvalidProblemId "Problem ID cannot be empty.")

    it "rejects invalid characters in problem id" $
      validateProblemId "A1" `shouldSatisfy` isLeft

  describe "toLanguageId" $ do
    it "accepts positive language id" $ do
      let result = toLanguageId 4001
      result `shouldSatisfy` isRight
    -- LanguageId から Int を取り出す関数があればそれを使う
    -- なければ fmap (\(LanguageId i) -> i) result `shouldBe` Right 4001 のようにパターンマッチ
    -- (ただし、LanguageId コンストラクタは使えないので、この方法は不可)
    -- => LanguageId 用の deconstructor 関数 (例: deLanguageId) を Types.hs に追加するのが良い

    it "rejects non-positive language id" $
      toLanguageId 0 `shouldBe` Left (InvalidLanguageId 0)

  describe "toSourceFile" $ do
    it "accepts non-empty file path" $ do
      let result = toSourceFile "Main.hs"
      result `shouldSatisfy` isRight
    -- SourceFile 用の deconstructor 関数 (例: deSourceFile) を Types.hs に追加するのが良い
    -- fmap (\(SourceFile fp) -> fp) result `shouldBe` Right "Main.hs"

    it "rejects empty file path" $
      toSourceFile "" `shouldBe` Left (SourceFileNotFound "")
