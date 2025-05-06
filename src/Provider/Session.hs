module Provider.Session (loadSessionIO, saveSessionIO) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Text.Encoding as TEnc
import Interface (HasFileSystem (..))
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import Types (AppError, Session (..), validateSession)
import Prelude hiding (readFile)

sessionFilePath :: (MonadIO m) => m FilePath
sessionFilePath = do
  home <- liftIO getHomeDirectory
  pure $ home </> ".local" </> "share" </> "atcoder-cil" </> "session.txt"

loadSessionIO :: (MonadIO m, HasFileSystem m) => m (Either AppError Session)
loadSessionIO =
  sessionFilePath
    >>= readFile
    >>= either
      (return . Left)
      (return . validateSession . TEnc.decodeUtf8)

saveSessionIO :: (MonadIO m, HasFileSystem m) => Session -> m (Either AppError ())
saveSessionIO (Session session) = do
  sessionPath <- sessionFilePath
  saveFile sessionPath $ TEnc.encodeUtf8 session