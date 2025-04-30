-- src/Infrastructure/FileSystem.hs
{-# LANGUAGE OverloadedStrings #-}

module Infra.FileSystem
  ( createDirectoryIO,
  )
where

import Control.Monad.Catch (try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text as T
import Domain.Types (DomainError (..)) -- InfraError を使う
import GHC.IO.Exception (IOException) -- 例外型
import qualified System.Directory as Dir

-- | IO implementation for createDirectory, handling exceptions.
createDirectoryIO :: (MonadIO m) => FilePath -> m (Either DomainError ())
createDirectoryIO path = do
  -- System.Directory の関数は IO アクションなので liftIO で持ち上げる
  -- try で IOException を捕捉する
  result <- liftIO $ try (Dir.createDirectoryIfMissing True path)
  case result of
    Right () -> pure $ Right ()
    -- 捕捉した IOException を DomainError (InfraError) に変換
    Left e -> pure $ Left (InfraError (T.pack $ show (e :: IOException)))
