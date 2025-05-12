module Usecase.DownloadSpec (spec) where

import Mock
import Test.Hspec
import Types
import Usecase.Download (download)

spec :: Spec
spec = describe "Usecase.Download.download" $ do
  -- テストデータ準備
  let task = Task (ContestId "abc100") (ProblemId "a")
  let tc1Name = "sample1"
  let tc1Input = "1 2\n"
  let tc1Output = "3\n"
  let tc1 = TestCase tc1Name tc1Input tc1Output
  let tc2Name = "sample2"
  let tc2Input = "10 20\n"
  let tc2Output = "30\n"
  let tc2 = TestCase tc2Name tc2Input tc2Output
  let testCases = [tc1, tc2]

  it "正常系: テストケースを取得し、./test ディレクトリに保存する" $ do
    -- 1. Arrange
    let initialState =
          initialMockState
            { msLoadTask = Right task,
              msFetchTestCases = Right testCases
            }
    -- 2. Act
    (result, finalState) <- execMockApp download initialState

    -- 3. Assert
    result `shouldBe` Right ()
    msSaveTestCase finalState `shouldBe` [tc1, tc2]

  it "異常系: タスクの取得に失敗した場合" $ do
    -- 1. Arrange
    let errorTask = Left (ProviderError "Invalid directory")
    let initialState =
          initialMockState
            { msLoadTask = errorTask,
              msFetchTestCases = Right testCases
            }
    -- 2. Act
    (result, finalState) <- execMockApp download initialState

    -- 3. Assert
    result `shouldBe` errorTask
    msSaveTestCase finalState `shouldBe` []

  it "異常系: テストケースの取得に失敗した場合" $ do
    -- 1. Arrange
    let fetchError = ProviderError "Network Error"
    let initialState =
          initialMockState
            { msLoadTask = Right task,
              msFetchTestCases = Left fetchError
            }
    -- 2. Act
    (result, finalState) <- execMockApp download initialState

    -- 3. Assert
    result `shouldBe` Left fetchError
    msSaveTestCase finalState `shouldBe` []