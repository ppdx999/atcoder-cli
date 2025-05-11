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