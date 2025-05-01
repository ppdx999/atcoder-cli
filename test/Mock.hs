{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- test/Test/Mock.hs
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
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Domain -- Domain.hs から型をインポート
import Usecase.Ports -- Capability をインポート

-- | モック実行時の状態
data MockState = MockState
  { msLogs :: [Text], -- 記録されたログメッセージ (順序保持)
    msCreatedDirs :: Set FilePath, -- 作成されたディレクトリの集合
    msProblemIdsResult :: Either DomainError [ProblemId], -- fetchProblemIds の結果を制御
    msCreateDirResult :: FilePath -> Either DomainError () -- createDirectory の結果を制御
  }

-- | モック状態の初期値
initialMockState :: MockState
initialMockState =
  MockState
    { msLogs = [],
      msCreatedDirs = Set.empty,
      msProblemIdsResult = Right [], -- デフォルトは成功(空リスト)
      msCreateDirResult = \_ -> Right () -- デフォルトは常に成功
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

  -- logError も必要なら実装
  logError msg = modify $ \s -> s {msLogs = msLogs s ++ ["[ERROR] " <> msg]}

instance HasFileSystem MockApp where
  createDirectory path = do
    resultFunc <- gets msCreateDirResult
    case resultFunc path of
      Right () -> do
        modify $ \s -> s {msCreatedDirs = Set.insert path (msCreatedDirs s)}
        pure $ Right ()
      Left err -> pure $ Left err

instance HasAtcoder MockApp where
  fetchProblemIds _contestId = gets msProblemIdsResult
