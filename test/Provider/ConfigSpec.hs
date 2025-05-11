module Provider.ConfigSpec (spec) where

import Mock
import Provider.Config (loadTaskIO)
import Test.Hspec (Spec, describe, it, shouldBe)
import Types (AppError (ProviderError), ContestId (..), ProblemId (..), Task (..))

spec :: Spec
spec = describe "Provider.Config" $ do
  describe "loadTaskIO" $ do
    it "正常系" $ do
      let initialState = initialMockState {msCurrentDir = "/path/to/abc100/a"}
      (result, _finalState) <- execMockApp loadTaskIO initialState
      result `shouldBe` Right (Task (ContestId "abc100") (ProblemId "a"))

    it "カレントディレクトリからパスの解析に失敗" $ do
      let initialState = initialMockState {msCurrentDir = "/invalid_contest"}
      (result, _finalState) <- execMockApp loadTaskIO initialState
      result `shouldBe` Left (ProviderError "Could not parse contest/problem ID from path: /invalid_contest")
