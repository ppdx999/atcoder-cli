module Provider.Executor (executeCmdIO) where

import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Functor ((<&>))
import Interface
import Provider.Utils (join, try)
import System.Process (readProcess)
import Types

executeCmdIO :: (HasLogger m, MonadIO m, MonadCatch m) => Cmd -> Stdin -> m (Either AppError Stdout)
executeCmdIO (Cmd cmd) (Stdin stdin) = do
  logInfo $ "execute command: " <> join " " cmd
  let filePath = concat $ take 1 cmd
  let opt = drop 1 cmd
  try (liftIO $ readProcess filePath opt stdin) <&> fmap Stdout
