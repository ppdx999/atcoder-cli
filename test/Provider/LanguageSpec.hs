module Provider.LanguageSpec (spec) where

import Mock
import Provider.Language (cpp, detectLanguageIO)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import TestUtils

spec :: Spec
spec = describe "Provider.Language" $ do
  describe "loadTaskIO" $ do
    it "正常系" $ do
      -- 1. Arrange
      let initialState =
            initialMockState
              { -- Mock HasFileSystem
                msReadDir = Right ["test", "main.cpp"]
              }
      -- 2. Act
      (result, _finalState) <- execMockApp detectLanguageIO initialState
      -- 3. Assert
      result `shouldBe` Right cpp

    it "readDirが古パスで結果を返しても正常に動く" $ do
      -- 1. Arrange
      let initialState =
            initialMockState
              { -- Mock HasFileSystem
                msReadDir = Right ["/tmp/abc100/a/test", "/tmp/abc100/a/main.cpp"]
              }
      -- 2. Act
      (result, _finalState) <- execMockApp detectLanguageIO initialState
      -- 3. Assert
      result `shouldBe` Right cpp

    it "複数の言語が存在する場合、Language.hsのlangsの順に探し、最初に見つかったものを選択する" $ do
      -- 1. Arrange
      let initialState =
            initialMockState
              { -- Mock HasFileSystem
                msReadDir = Right ["test", "main.cpp", "main.py"]
              }
      -- 2. Act
      (result, _finalState) <- execMockApp detectLanguageIO initialState
      -- 3. Assert
      result `shouldBe` Right cpp

    it "指定された言語が存在しない場合はProviderErrorを返す" $ do
      -- 1. Arrange
      let initialState =
            initialMockState
              { -- Mock HasFileSystem
                msReadDir = Right ["test"]
              }
      -- 2. Act
      (result, _finalState) <- execMockApp detectLanguageIO initialState
      -- 3. Assert
      result `shouldSatisfy` isProviderError