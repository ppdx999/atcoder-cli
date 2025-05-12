module Usecase.TestSpec (spec) where

import Mock
import Provider.Language (cpp)
import Test.Hspec
import Types
import Usecase.Test (test)

spec :: Spec
spec = describe "Usecase.Test" $ do
  let tc1 = TestCase "1" "" ""
  let tc2 = TestCase "2" "" ""
  let lang = cpp
  let runResult = RunTestCaseResult "OK"
  it "正常系" $ do
    -- Arrange
    let initialState =
          initialMockState
            { msDetectLanguage = Right lang,
              msLoadTestCases = Right [tc1, tc2],
              msRunTestCaseResult = Right runResult
            }
    -- Act
    (result, finalState) <- execMockApp test initialState
    -- Assert
    result `shouldBe` Right ()

    msBuildLanguage finalState `shouldBe` [lang]
    msRunTestCaseArgs finalState `shouldBe` [(lang, tc1), (lang, tc2)]
    msCleanupBuiltFile finalState `shouldBe` [lang]
    msReportTestResult finalState `shouldBe` [(tc1, runResult), (tc2, runResult)]