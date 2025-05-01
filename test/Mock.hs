-- test/Mock.hs
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Mock
  ( MockState (..),
    initialMockState,
    MockApp (..),
    execMockApp,
  )
where

import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.State.Strict (MonadState, StateT (..), gets, modify, runStateT)
import Data.ByteString (ByteString) -- Import ByteString
import Data.Map (Map) -- Import Map
import qualified Data.Map as Map -- Import Map qualified
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
-- Types.hs から型をインポート
import Interface -- Capability をインポート
import Types

-- | モック実行時の状態
data MockState = MockState
  { msLogs :: [Text], -- 記録されたログメッセージ (順序保持)
    msCreatedDirs :: Set FilePath, -- 作成されたディレクトリの集合
    msProblemIdsResult :: Either AppError [ProblemId], -- fetchProblemIds の結果を制御
    msCreateDirResult :: FilePath -> Either AppError (), -- createDirectory の結果を制御
    -- Download 用に追加
    msCurrentDir :: FilePath, -- getCurrentDirectory の結果を制御
    msTestCasesResult :: Either AppError [TestCase], -- fetchTestCases の結果を制御
    msSaveFileResult :: FilePath -> ByteString -> Either AppError (), -- saveFile の結果を制御
    msSavedFiles :: Map FilePath ByteString -- 保存されたファイルの内容を記録
  }

-- | モック状態の初期値
initialMockState :: MockState
initialMockState =
  MockState
    { msLogs = [],
      msCreatedDirs = Set.empty,
      msProblemIdsResult = Right [],
      msCreateDirResult = \_ -> Right (),
      -- Download 用に追加
      msCurrentDir = "/tmp/abc100/a", -- デフォルトのカレントディレクトリ
      msTestCasesResult = Right [], -- デフォルトは成功(空リスト)
      msSaveFileResult = \_ _ -> Right (), -- デフォルトは常に成功
      msSavedFiles = Map.empty -- 最初は空
    }

-- | モック用のモナド
newtype MockApp a = MockApp {runMockApp :: StateT MockState IO a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadState MockState,
      MonadThrow
    )

-- | MockApp で Capability を実行するためのヘルパー
execMockApp :: MockApp a -> MockState -> IO (a, MockState)
execMockApp = runStateT . runMockApp

-- | Capability のインスタンス実装
instance HasLogger MockApp where
  logInfo msg = modify $ \s -> s {msLogs = msLogs s ++ [msg]}
  logError msg = modify $ \s -> s {msLogs = msLogs s ++ ["[ERROR] " <> msg]}

instance HasFileSystem MockApp where
  createDirectory path = do
    resultFunc <- gets msCreateDirResult
    case resultFunc path of
      Right () -> do
        modify $ \s -> s {msCreatedDirs = Set.insert path (msCreatedDirs s)}
        pure $ Right ()
      Left err -> pure $ Left err

  -- getCurrentDirectory の実装を追加
  getCurrentDirectory = gets msCurrentDir

  -- saveFile の実装を追加
  saveFile path content = do
    resultFunc <- gets msSaveFileResult
    case resultFunc path content of
      Right () -> do
        -- 成功したら保存内容を記録
        modify $ \s -> s {msSavedFiles = Map.insert path content (msSavedFiles s)}
        pure $ Right ()
      Left err -> pure $ Left err

instance HasAtcoder MockApp where
  fetchProblemIds _contestId = gets msProblemIdsResult

  -- fetchTestCases の実装を追加
  fetchTestCases _task = gets msTestCasesResult
