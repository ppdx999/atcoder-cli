module Provider.LanguageSpec (spec) where

import Mock
import Provider.Language (cpp, detectLanguageIO)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import TestUtils

spec :: Spec
spec = describe "Provider.Language" $ do
  describe "loadTaskIO" $ do
    it "正常系" $ do
      let initialState = initialMockState {msReadDir = Right ["test", "main.cpp"]}
      (result, _finalState) <- execMockApp detectLanguageIO initialState
      result `shouldBe` Right cpp

    it "readDirが古パスで結果を返しても正常に動く" $ do
      let initialState = initialMockState {msReadDir = Right ["/tmp/abc100/a/test", "/tmp/abc100/a/main.cpp"]}
      (result, _finalState) <- execMockApp detectLanguageIO initialState
      result `shouldBe` Right cpp

    it "複数の言語が存在する場合、Language.hsのlangsの順に探し、最初に見つかったものを選択する" $ do
      let initialState = initialMockState {msReadDir = Right ["test", "main.cpp", "main.py"]}
      (result, _finalState) <- execMockApp detectLanguageIO initialState
      result `shouldBe` Right cpp

    it "指定された言語が存在しない場合はProviderErrorを返す" $ do
      let initialState = initialMockState {msReadDir = Right ["test"]}
      (result, _finalState) <- execMockApp detectLanguageIO initialState
      result `shouldSatisfy` isProviderError