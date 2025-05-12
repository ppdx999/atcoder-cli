module Provider.ConfigSpec (spec) where

import Mock
import Provider.Config (loadTaskIO)
import Test.Hspec (Spec, describe, it, shouldBe)
import Types (AppError (ProviderError), ContestId (..), ProblemId (..), Task (..))

spec :: Spec
spec = describe "Provider.Config" $ do
  describe "loadTaskIO" $ do
    it "正常系" $ do
      -- 1. Arrange
      let initialState =
            initialMockState
              { -- HasFileSystem
                msGetCurrentDirectory = "/path/to/abc100/a"
              }
      -- 2. Act
      (result, _finalState) <- execMockApp loadTaskIO initialState
      -- 3. Assert
      result `shouldBe` Right (Task (ContestId "abc100") (ProblemId "a"))

    it "カレントディレクトリからパスの解析に失敗" $ do
      -- 1. Arrange
      let initialState =
            initialMockState
              { -- HasFileSystem
                msGetCurrentDirectory = "/invalid_contest"
              }
      -- 2. Act
      (result, _finalState) <- execMockApp loadTaskIO initialState
      -- 3. Assert
      result `shouldBe` Left (ProviderError "Could not parse contest/problem ID from path: /invalid_contest")
