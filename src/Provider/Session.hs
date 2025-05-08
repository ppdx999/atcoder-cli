module Provider.Session (loadSessionIO, saveSessionIO) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import qualified Data.Text.Encoding as TEnc
import Interface (HasFileSystem (..))
import System.Directory (getHomeDirectory)
import System.FilePath (takeDirectory, (</>))
import Types (AppError (SessionNotFound), Session (..), validateSession)
import Prelude hiding (readFile)

sessionFilePath :: (MonadIO m) => m FilePath
sessionFilePath = do
  home <- liftIO getHomeDirectory
  pure $ home </> ".local" </> "share" </> "atcoder-cli" </> "session.txt"

loadSessionIO :: (MonadIO m, HasFileSystem m) => m (Either AppError Session)
loadSessionIO = do
  sessionPath <- sessionFilePath
  isExist <- doesFileExist sessionPath
  if isExist
    then
      readFile sessionPath
        >>= either (return . Left) (return . validateSession . TEnc.decodeUtf8)
    else
      return $ Left SessionNotFound

saveSessionIO :: (MonadIO m, HasFileSystem m) => Session -> m (Either AppError ())
saveSessionIO (Session session) = runExceptT $ do
  sessionPath <- sessionFilePath
  _ <- ExceptT $ createDirectoryIfMissing True $ takeDirectory sessionPath
  ExceptT $ saveFile sessionPath $ TEnc.encodeUtf8 session