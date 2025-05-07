module Provider.FileSystem
  ( createDirectoryIO,
    getCurrentDirectoryIO,
    readFileIO,
    saveFileIO,
  )
where

import Control.Monad.Catch (try)
import Control.Monad.IO.Class (MonadIO, liftIO)
-- ProviderError を使う
-- 例外型

import qualified Data.ByteString as BS
import qualified Data.Text as T
import GHC.IO.Exception (IOException)
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

getCurrentDirectoryIO :: (MonadIO m) => m FilePath
getCurrentDirectoryIO = liftIO Dir.getCurrentDirectory

readFileIO :: FilePath -> IO (Either AppError BS.ByteString)
readFileIO path =
  try (BS.readFile path)
    >>= either
      (\e -> pure $ Left (ProviderError (T.pack $ show (e :: IOException))))
      (pure . Right)

-- ファイル保存の IO 実装
saveFileIO :: (MonadIO m) => FilePath -> BS.ByteString -> m (Either AppError ())
saveFileIO path content = do
  result <- liftIO $ try (BS.writeFile path content)
  case result of
    Right () -> pure $ Right ()
    Left e -> pure $ Left (ProviderError (T.pack $ show (e :: IOException)))
