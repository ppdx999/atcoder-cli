module Provider.Session (loadSessionIO, saveSessionIO) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (MonadTrans (lift))
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import Data.Functor ((<&>))
import qualified Data.Text.Encoding as TEnc
import Interface (HasConfig (..), HasFileSystem (..))
import System.FilePath (takeDirectory)
import Types (AppError (SessionNotFound), Session (..), validateSession)
import Prelude hiding (readFile)

loadSessionIO :: (MonadIO m, HasFileSystem m, HasConfig m) => m (Either AppError Session)
loadSessionIO = runExceptT $ do
  sessionPath <- ExceptT loadSessionPath
  isExist <- lift $ doesFileExist sessionPath
  ExceptT $
    if isExist
      then
        readFile sessionPath
          <&> either Left (validateSession . TEnc.decodeUtf8)
      else
        return (Left SessionNotFound)

saveSessionIO :: (MonadIO m, HasFileSystem m, HasConfig m) => Session -> m (Either AppError ())
saveSessionIO (Session session) = runExceptT $ do
  sessionPath <- ExceptT loadSessionPath
  _ <- ExceptT $ createDirectoryIfMissing True $ takeDirectory sessionPath
  ExceptT $ saveFile sessionPath $ TEnc.encodeUtf8 session