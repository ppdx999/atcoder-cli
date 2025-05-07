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

createDirectoryIO :: FilePath -> IO (Either AppError ())
createDirectoryIO path = do
  result <- liftIO $ try (Dir.createDirectoryIfMissing True path)
  case result of
    Right () -> pure $ Right ()
    Left e -> pure $ Left (ProviderError (T.pack $ show (e :: IOException)))

getCurrentDirectoryIO :: IO FilePath
getCurrentDirectoryIO = liftIO Dir.getCurrentDirectory

readFileIO :: FilePath -> IO (Either AppError BS.ByteString)
readFileIO path =
  try (BS.readFile path)
    >>= either
      (\e -> pure $ Left (ProviderError (T.pack $ show (e :: IOException))))
      (pure . Right)

-- ファイル保存の IO 実装
saveFileIO :: FilePath -> BS.ByteString -> IO (Either AppError ())
saveFileIO path content = do
  result <- liftIO $ try (BS.writeFile path content)
  case result of
    Right () -> pure $ Right ()
    Left e -> pure $ Left (ProviderError (T.pack $ show (e :: IOException)))
