module Provider.Config (loadConfigIO, loadSessionPathIO) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import Types (AppError, Config (..))

loadConfigIO :: IO (Either AppError Config)
loadConfigIO = do
  sessionFilePath <- sessionFilePathIO
  return $
    Right
      Config
        { sessionPath = sessionFilePath
        }

loadSessionPathIO :: IO (Either AppError FilePath)
loadSessionPathIO = fmap (fmap sessionPath) loadConfigIO

sessionFilePathIO :: IO FilePath
sessionFilePathIO = do
  home <- liftIO getHomeDirectory
  pure $ home </> ".local" </> "share" </> "atcoder-cli" </> "session.txt"
