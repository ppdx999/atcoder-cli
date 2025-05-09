-- src/Usecase/Download.hs
{-# LANGUAGE OverloadedStrings #-}

module Usecase.Download
  ( download,
  )
where

import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Except (ExceptT (..))
import Data.Foldable (traverse_)
import qualified Data.Text as T
import Interface
import System.FilePath ((</>))
import Types

download ::
  ( HasLogger m,
    HasFileSystem m,
    HasAtcoder m,
    HasConfig m
  ) =>
  ExceptT AppError m ()
download = do
  logInfoE "Starting download..."

  task@( Task
           (ContestId cid)
           (ProblemId pid)
         ) <-
    ExceptT loadTask
  logInfoE $ "Target: Contest=" <> cid <> ", Problem=" <> pid

  -- 2. テストケースを取得
  logInfoE "Fetching test cases..."
  testCases <- ExceptT $ fetchTestCases task
  logInfoE $ "Found " <> T.pack (show $ length testCases) <> " test cases."

  -- 3. 各テストケースを保存
  testDir <- ExceptT loadTestDir
  logInfoE $ "Saving test cases to " <> T.pack testDir <> " ..."

  -- ./test ディレクトリを作成 (存在していてもエラーにならないようにする)
  -- createDirectory は Either を返すので ExceptT でラップ
  -- 既に存在する場合のエラーは無視したいが、ここでは一旦そのまま
  ExceptT $ createDirectory testDir

  -- 各テストケースをループして保存
  traverse_ (saveSingleTestCase testDir) testCases

  lift $ logInfo "Download complete."
  where
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

    logInfoE :: (HasLogger m, MonadTrans t) => T.Text -> t m ()
    logInfoE = lift . logInfo
