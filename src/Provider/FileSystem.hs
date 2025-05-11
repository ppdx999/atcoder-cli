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

import Control.Monad.IO.Class (liftIO)
import Provider.Utils (try)
import qualified System.Directory as Dir
import Types (AppError (..))

createDirectoryIO :: FilePath -> IO (Either AppError ())
createDirectoryIO = try . Dir.createDirectoryIfMissing True

createDirectoryIfMissingIO :: Bool -> FilePath -> IO (Either AppError ())
createDirectoryIfMissingIO = (try .) . Dir.createDirectoryIfMissing

getCurrentDirectoryIO :: IO FilePath
getCurrentDirectoryIO = Dir.getCurrentDirectory

readFileIO :: FilePath -> IO (Either AppError String)
readFileIO = try . readFile

saveFileIO :: FilePath -> String -> IO (Either AppError ())
saveFileIO = (try .) . writeFile

removeFileIO :: FilePath -> IO (Either AppError ())
removeFileIO = try . Dir.removeFile

readDirIO :: FilePath -> IO (Either AppError [FilePath])
readDirIO = try . Dir.getDirectoryContents

doesFileExistIO :: FilePath -> IO Bool
doesFileExistIO = liftIO . Dir.doesFileExist