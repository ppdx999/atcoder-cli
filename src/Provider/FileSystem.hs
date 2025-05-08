module Provider.FileSystem
  ( createDirectoryIO,
    createDirectoryIfMissingIO,
    getCurrentDirectoryIO,
    readFileIO,
    saveFileIO,
    doesFileExistIO,
  )
where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import Provider.Utils (try)
import qualified System.Directory as Dir
import Types (AppError (..))

createDirectoryIO :: FilePath -> IO (Either AppError ())
createDirectoryIO path = try (Dir.createDirectoryIfMissing True path)

createDirectoryIfMissingIO :: Bool -> FilePath -> IO (Either AppError ())
createDirectoryIfMissingIO missing path = try (Dir.createDirectoryIfMissing missing path)

getCurrentDirectoryIO :: IO FilePath
getCurrentDirectoryIO = Dir.getCurrentDirectory

readFileIO :: FilePath -> IO (Either AppError BS.ByteString)
readFileIO path = try (BS.readFile path)

saveFileIO :: FilePath -> BS.ByteString -> IO (Either AppError ())
saveFileIO path content = try (BS.writeFile path content)

doesFileExistIO :: FilePath -> IO Bool
doesFileExistIO path = liftIO (Dir.doesFileExist path)