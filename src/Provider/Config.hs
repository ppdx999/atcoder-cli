module Provider.Config (loadSessionPathIO) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import Types (AppError)

loadSessionPathIO :: IO (Either AppError FilePath)
loadSessionPathIO = Right <$> sessionFilePathIO

sessionFilePathIO :: IO FilePath
sessionFilePathIO = do
  home <- liftIO getHomeDirectory
  pure $ home </> ".local" </> "share" </> "atcoder-cli" </> "session.txt"
