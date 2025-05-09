-- test/Usecase/DownloadSpec.hs
{-# LANGUAGE OverloadedStrings #-}

module Usecase.DownloadSpec (spec) where

import Control.Monad.Trans.Except (runExceptT)
import qualified Data.ByteString.Char8 as BSC
import Mock
import Test.Hspec
import Types
import Usecase.Download (download)

spec :: Spec
spec = describe "Usecase.Download.download" $ do
  -- テストデータ準備
  let task = Task (ContestId "abc100") (ProblemId "a")
  let tc1Name = "sample1"
  let tc1Input = BSC.pack "1 2\n"
  let tc1Output = BSC.pack "3\n"
  let tc1 = TestCase tc1Name tc1Input tc1Output
  let tc2Name = "sample2"
  let tc2Input = BSC.pack "10 20\n"
  let tc2Output = BSC.pack "30\n"
  let tc2 = TestCase tc2Name tc2Input tc2Output
  let testCases = [tc1, tc2]

  it "正常系: テストケースを取得し、./test ディレクトリに保存する" $ do
    let initialState =
          initialMockState
            { msTask = Right task,
              msTestCasesResult = Right testCases
            }
    (result, finalState) <- execMockApp (runExceptT download) initialState

    -- 結果の検証
    result `shouldBe` Right ()

    -- 保存されたセッションの検証
    msSavedTestCase finalState `shouldBe` [tc1, tc2]

  it "異常系: タスクの取得に失敗した場合" $ do
    let errorTask = Left (ProviderError "Invalid directory")
    let initialState =
          initialMockState
            { msTask = errorTask,
              msTestCasesResult = Right testCases
            }
    (result, finalState) <- execMockApp (runExceptT download) initialState

    -- 結果の検証 (parseTaskFromPath が返すエラー)
    result `shouldBe` errorTask

    -- 保存されたセッションは存在しない
    msSavedSessions finalState `shouldBe` []

  it "異常系: テストケースの取得に失敗した場合" $ do
    let fetchError = ProviderError "Network Error"
    let initialState =
          initialMockState
            { msTask = Right task,
              msTestCasesResult = Left fetchError
            }
    (result, finalState) <- execMockApp (runExceptT download) initialState

    -- 結果の検証
    result `shouldBe` Left fetchError

    -- 保存されたセッションは存在しない
    msSavedSessions finalState `shouldBe` []

  it "異常系: テストケースファイルの保存に失敗した場合" $ do
    let saveError = ProviderError "Disk full"
    let initialState =
          initialMockState
            { msTask = Right task,
              msTestCasesResult = Right testCases,
              msSaveTestCaseFn = \_ -> Left saveError
            }
    (result, finalState) <- execMockApp (runExceptT download) initialState

    -- 結果の検証
    result `shouldBe` Left saveError

    -- 保存されたセッションは存在しない
    msSavedSessions finalState `shouldBe` []