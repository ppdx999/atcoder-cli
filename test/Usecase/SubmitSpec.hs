module Usecase.SubmitSpec (spec) where

import Mock
import Provider.Language (cpp)
import Test.Hspec
import Types
import Usecase.Submit (submit)

spec :: Spec
spec = describe "Usecase.Submit" $ do
  let lang = cpp
  it "正常系" $ do
    -- Arrange
    let initialState =
          initialMockState
            { msDetectLanguage = Right lang,
              msReadFile = Right "source code",
              msLoadTask = Right (Task (ContestId "abc100") (ProblemId "a")),
              msSubmitPageUrl = "https://example.com"
            }
    -- Act
    (result, finalState) <- execMockApp submit initialState
    -- Assert
    result `shouldBe` Right ()
    msSetClipboard finalState `shouldBe` ["source code"]
