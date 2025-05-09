-- test/Usecase/DownloadSpec.hs
{-# LANGUAGE OverloadedStrings #-}

module Usecase.DownloadSpec (spec) where

import Control.Monad.Trans.Except (runExceptT)
import qualified Data.ByteString.Char8 as BSC -- For ByteString literals
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Mock -- モック関連をインポート
import System.FilePath ((</>))
import Test.Hspec
import Types
import Usecase.Download (download)

spec :: Spec
spec = describe "Usecase.Download.download" $ do
  -- テストデータ準備
  let currentDir = "/path/to/abc100/a"
  let testDir = currentDir </> "test"
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
  let tc1InPath = testDir </> T.unpack tc1Name <> ".in"
  let tc1OutPath = testDir </> T.unpack tc1Name <> ".out"
  let tc2InPath = testDir </> T.unpack tc2Name <> ".in"
  let tc2OutPath = testDir </> T.unpack tc2Name <> ".out"

  it "正常系: テストケースを取得し、./test ディレクトリに保存する" $ do
    let initialState =
          initialMockState
            { msTestDir = Right testDir,
              msTask = Right task,
              msTestCasesResult = Right testCases
            }
    (result, finalState) <- execMockApp (runExceptT download) initialState

    -- 結果の検証
    result `shouldBe` Right ()

    -- 作成されたディレクトリの検証
    msCreatedDirs finalState `shouldBe` Set.singleton testDir

    -- 保存されたファイルの検証
    msSavedFiles finalState
      `shouldBe` Map.fromList
        [ (tc1InPath, tc1Input),
          (tc1OutPath, tc1Output),
          (tc2InPath, tc2Input),
          (tc2OutPath, tc2Output)
        ]

  it "異常系: タスクの取得に失敗した場合" $ do
    let errorTask = Left (ProviderError "Invalid directory")
    let initialState =
          initialMockState
            { msTestDir = Right testDir,
              msTask = errorTask,
              msTestCasesResult = Right testCases
            }
    (result, finalState) <- execMockApp (runExceptT download) initialState

    -- 結果の検証 (parseTaskFromPath が返すエラー)
    result `shouldBe` errorTask

    -- ディレクトリやファイルが作成/保存されていないこと
    msCreatedDirs finalState `shouldBe` Set.empty
    msSavedFiles finalState `shouldBe` Map.empty

  it "異常系: テストケースの取得に失敗した場合" $ do
    let fetchError = ProviderError "Network Error"
    let initialState =
          initialMockState
            { msTestDir = Right testDir,
              msTask = Right task,
              msTestCasesResult = Left fetchError
            }
    (result, finalState) <- execMockApp (runExceptT download) initialState

    -- 結果の検証
    result `shouldBe` Left fetchError

    -- ログの検証 (中断されていること)
    msLogs finalState `shouldContain` ["Fetching test cases..."]
    msLogs finalState `shouldNotContain` ["Saving test cases to"]

    -- ディレクトリやファイルが作成/保存されていないこと
    msCreatedDirs finalState `shouldBe` Set.empty
    msSavedFiles finalState `shouldBe` Map.empty

  it "異常系: テストケースファイルの保存に失敗した場合" $ do
    let saveError = ProviderError "Disk full"
    -- tc1OutPath の保存時のみエラーを返すように設定
    let resultFunc path _content = if path == tc1OutPath then Left saveError else Right ()
    let initialState =
          initialMockState
            { msTestDir = Right testDir,
              msTask = Right task,
              msTestCasesResult = Right testCases,
              msSaveFileResult = resultFunc
            }
    (result, finalState) <- execMockApp (runExceptT download) initialState

    -- 結果の検証
    result `shouldBe` Left saveError

    -- ログの検証 (失敗したファイルのログまで)
    msLogs finalState `shouldContain` ["Saving " <> T.pack tc1InPath]
    msLogs finalState `shouldContain` ["Saving " <> T.pack tc1OutPath]
    msLogs finalState `shouldNotContain` ["Saving " <> T.pack tc2InPath] -- 後続のファイル保存は実行されない

    -- 作成されたディレクトリの検証
    msCreatedDirs finalState `shouldBe` Set.singleton testDir

    -- 保存されたファイルの検証 (失敗したファイルとその後のファイルは含まれない)
    msSavedFiles finalState `shouldBe` Map.fromList [(tc1InPath, tc1Input)]
