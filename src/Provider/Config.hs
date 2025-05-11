module Provider.Config (loadSessionPathIO, loadTaskIO, loadTestDirIO) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Interface (HasFileSystem (getCurrentDirectory))
import System.Directory (getHomeDirectory)
import System.FilePath (pathSeparator, takeBaseName, takeDirectory, (</>))
import Types (AppError (..), Task (..), validateContestId, validateProblemId)

loadSessionPathIO :: IO (Either AppError FilePath)
loadSessionPathIO = Right <$> sessionFilePathIO

loadTaskIO :: (MonadIO m, HasFileSystem m) => m (Either AppError Task)
loadTaskIO = do
  currentDir <- getCurrentDirectory

  let normalizedPath = dropTrailingSeparator currentDir
  let pidStr = takeBaseName normalizedPath
  let cidStr = takeBaseName (takeDirectory normalizedPath)

  return $
    if null cidStr || null pidStr
      then
        Left
          ( ProviderError
              ("Could not parse contest/problem ID from path: " <> currentDir)
          )
      else do
        contestId <- validateContestId cidStr
        problemId <- validateProblemId pidStr
        Right Task {taskContestId = contestId, taskProblemId = problemId}
  where
    dropTrailingSeparator :: FilePath -> FilePath
    dropTrailingSeparator p =
      if isTrailingSeparator p && length p > 1
        then take (length p - 1) p
        else p
      where
        isTrailingSeparator path = case reverse path of
          (c : _) -> c == pathSeparator
          [] -> False

loadTestDirIO :: (MonadIO m, HasFileSystem m) => m (Either AppError FilePath)
loadTestDirIO = do
  currentDir <- getCurrentDirectory
  return $ Right (currentDir </> "test")

sessionFilePathIO :: IO FilePath
sessionFilePathIO = do
  home <- liftIO getHomeDirectory
  pure $ home </> ".local" </> "share" </> "atcoder-cli" </> "session.txt"
