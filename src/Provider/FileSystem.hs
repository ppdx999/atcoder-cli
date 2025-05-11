module Provider.FileSystem
  ( createDirectoryIO,
    createDirectoryIfMissingIO,
    getCurrentDirectoryIO,
    readFileIO,
    saveFileIO,
    removeFileIO,
    readDirIO,
    doesFileExistIO,
  )
where

import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Interface (HasLogger (..))
import Provider.Utils (try)
import qualified System.Directory as Dir
import Types (AppError (..))

createDirectoryIO :: (HasLogger m, MonadIO m, MonadCatch m) => FilePath -> m (Either AppError ())
createDirectoryIO path = do
  logInfo $ "create directory: " <> path
  try . liftIO . Dir.createDirectoryIfMissing True $ path

createDirectoryIfMissingIO :: (HasLogger m, MonadIO m, MonadCatch m) => Bool -> FilePath -> m (Either AppError ())
createDirectoryIfMissingIO missing path = do
  logInfo $ "create directory if missing: " <> path
  try $ liftIO $ Dir.createDirectoryIfMissing missing path

getCurrentDirectoryIO :: (HasLogger m, MonadIO m) => m FilePath
getCurrentDirectoryIO = do
  logInfo "get current directory..."
  liftIO Dir.getCurrentDirectory

readFileIO :: (HasLogger m, MonadIO m, MonadCatch m) => FilePath -> m (Either AppError String)
readFileIO path = do
  logInfo $ "read file: " <> path
  try $ liftIO $ readFile path

saveFileIO :: (HasLogger m, MonadIO m, MonadCatch m) => FilePath -> String -> m (Either AppError ())
saveFileIO path content = do
  logInfo $ "save file: " <> path
  try $ liftIO $ writeFile path content

removeFileIO :: (HasLogger m, MonadIO m, MonadCatch m) => FilePath -> m (Either AppError ())
removeFileIO path = do
  logInfo $ "remove file: " <> path
  try $ liftIO $ Dir.removeFile path

readDirIO :: (HasLogger m, MonadIO m, MonadCatch m) => FilePath -> m (Either AppError [FilePath])
readDirIO path = do
  logInfo $ "read directory: " <> path
  try $ liftIO $ Dir.getDirectoryContents path

doesFileExistIO :: (HasLogger m, MonadIO m) => FilePath -> m Bool
doesFileExistIO path = do
  logInfo $ "does file exist..." <> path
  liftIO $ Dir.doesFileExist path