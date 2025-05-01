-- src/Usecase/Download.hs
{-# LANGUAGE OverloadedStrings #-}

module Usecase.Download
  ( download,
  )
where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..))
import Data.Foldable (traverse_)
import qualified Data.Text as T
import Interface
import System.FilePath ((</>))
import Types
import Usecase.Utils (parseTaskFromPath)

download ::
  ( HasLogger m,
    HasFileSystem m,
    HasAtcoder m
  ) =>
  ExceptT AppError m ()
download = do
  lift $ logInfo "Starting download..."

  -- 1. カレントディレクトリから Task を取得
  currentDir <- lift getCurrentDirectory
  task <- ExceptT $ pure $ parseTaskFromPath currentDir
  let Task contestId problemId = task
  lift $ logInfo $ "Target: Contest=" <> deContestId contestId <> ", Problem=" <> deProblemId problemId

  -- 2. テストケースを取得
  lift $ logInfo "Fetching test cases..."
  testCases <- ExceptT $ fetchTestCases task
  lift $ logInfo $ "Found " <> T.pack (show $ length testCases) <> " test cases."

  -- 3. 各テストケースを保存
  let testDir = currentDir </> "test" -- 保存先ディレクトリ
  lift $ logInfo $ "Saving test cases to " <> T.pack testDir <> " ..."

  -- ./test ディレクトリを作成 (存在していてもエラーにならないようにする)
  -- createDirectory は Either を返すので ExceptT でラップ
  -- 既に存在する場合のエラーは無視したいが、ここでは一旦そのまま
  ExceptT $ createDirectory testDir

  -- 各テストケースをループして保存
  traverse_ (saveSingleTestCase testDir) testCases

  lift $ logInfo "Download complete."

-- ヘルパー関数: 1つのテストケースを保存 (入力と出力)
saveSingleTestCase ::
  ( HasLogger m,
    HasFileSystem m
  ) =>
  FilePath -> -- 保存先ベースディレクトリ (./test)
  TestCase ->
  ExceptT AppError m ()
saveSingleTestCase baseDir testCase = do
  let name = tcName testCase -- "sample1" など
  let inFile = baseDir </> T.unpack name <> ".in"
  let outFile = baseDir </> T.unpack name <> ".out"

  lift $ logInfo $ "Saving " <> T.pack inFile
  ExceptT $ saveFile inFile (tcInput testCase)

  lift $ logInfo $ "Saving " <> T.pack outFile
  ExceptT $ saveFile outFile (tcOutput testCase)
