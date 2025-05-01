-- test/Usecase/InitSpec.hs
{-# LANGUAGE OverloadedStrings #-}

module Usecase.InitSpec (spec) where

import Control.Monad.Trans.Except (runExceptT)
import qualified Data.Set as Set
import Mock
import System.FilePath ((</>))
import Test.Hspec
import Types
import Usecase.Init (initContest)

spec :: Spec
spec = describe "Usecase.Init.initContest" $ do
  let unsafeGetRight (Right x) = x
      unsafeGetRight (Left e) = error ("Test setup failed: " ++ show e)
  let contestId = unsafeGetRight $ toContestId "abc999"
  let contestDir = "abc999"
  let problemIdA = unsafeGetRight $ toProblemId "a"
  let problemIdB = unsafeGetRight $ toProblemId "b"
  let problems = [problemIdA, problemIdB]
  let problemDirA = contestDir </> "a"
  let problemDirB = contestDir </> "b"

  it "正常系: コンテストディレクトリと問題ディレクトリを作成し、ログを出力する" $ do
    let initialState = initialMockState {msProblemIdsResult = Right problems}
    (result, finalState) <- execMockApp (runExceptT (initContest contestId)) initialState

    -- 結果の検証
    result `shouldBe` Right ()

    -- ログの検証 (期待されるログメッセージ)
    msLogs finalState
      `shouldBe` [ "Initializing contest: abc999",
                   "Creating directory: abc999",
                   "Fetching problem list...",
                   "Found 2 problems.",
                   "Creating directory: abc999/a",
                   "Creating directory: abc999/b"
                 ]

    -- 作成されたディレクトリの検証
    msCreatedDirs finalState `shouldBe` Set.fromList [contestDir, problemDirA, problemDirB]

  it "異常系: 問題リストの取得に失敗した場合、エラーを返し処理を中断する" $ do
    let fetchError = ProviderError "Network timeout"
    let initialState = initialMockState {msProblemIdsResult = Left fetchError}
    (result, finalState) <- execMockApp (runExceptT (initContest contestId)) initialState

    -- 結果の検証
    result `shouldBe` Left fetchError

    -- ログの検証 (中断されていることを確認)
    msLogs finalState
      `shouldBe` [ "Initializing contest: abc999",
                   "Creating directory: abc999",
                   "Fetching problem list..."
                 ]

    -- 作成されたディレクトリの検証 (コンテストディレクトリのみ作成試行)
    msCreatedDirs finalState `shouldBe` Set.fromList [contestDir]

  it "異常系: コンテストディレクトリの作成に失敗した場合、エラーを返し処理を中断する" $ do
    let createError = ProviderError "Permission denied"
    let resultFunc path = if path == contestDir then Left createError else Right ()
    let initialState = initialMockState {msCreateDirResult = resultFunc}
    (result, finalState) <- execMockApp (runExceptT (initContest contestId)) initialState

    -- 結果の検証
    result `shouldBe` Left createError

    -- ログの検証 (中断されていることを確認)
    msLogs finalState
      `shouldBe` [ "Initializing contest: abc999",
                   "Creating directory: abc999"
                 ]

    -- 作成されたディレクトリの検証 (何も作成されていない)
    msCreatedDirs finalState `shouldBe` Set.empty

  it "異常系: 問題ディレクトリの作成に失敗した場合、エラーを返し処理を中断する" $ do
    let createError = ProviderError "Disk full"
    -- problemDirB の作成時のみエラーを返すように設定
    let resultFunc path = if path == problemDirB then Left createError else Right ()
    let initialState = initialMockState {msProblemIdsResult = Right problems, msCreateDirResult = resultFunc}
    (result, finalState) <- execMockApp (runExceptT (initContest contestId)) initialState

    -- 結果の検証
    result `shouldBe` Left createError

    -- ログの検証 (失敗したディレクトリ作成のログまで記録される)
    msLogs finalState
      `shouldBe` [ "Initializing contest: abc999",
                   "Creating directory: abc999",
                   "Fetching problem list...",
                   "Found 2 problems.",
                   "Creating directory: abc999/a",
                   "Creating directory: abc999/b"
                 ]

    -- 作成されたディレクトリの検証 (失敗したディレクトリは含まれない)
    msCreatedDirs finalState `shouldBe` Set.fromList [contestDir, problemDirA]
