-- src/Providerstructure/FileSystem.hs
{-# LANGUAGE OverloadedStrings #-}

module Provider.FileSystem
  ( createDirectoryIO,
  )
where

import Control.Monad.Catch (try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text as T
-- ProviderError を使う
import GHC.IO.Exception (IOException) -- 例外型
import qualified System.Directory as Dir
import Types (AppError (..))

-- | IO implementation for createDirectory, handling exceptions.
createDirectoryIO :: (MonadIO m) => FilePath -> m (Either AppError ())
createDirectoryIO path = do
  -- System.Directory の関数は IO アクションなので liftIO で持ち上げる
  -- try で IOException を捕捉する
  result <- liftIO $ try (Dir.createDirectoryIfMissing True path)
  case result of
    Right () -> pure $ Right ()
    -- 捕捉した IOException を AppError (ProviderError) に変換
    Left e -> pure $ Left (ProviderError (T.pack $ show (e :: IOException)))
