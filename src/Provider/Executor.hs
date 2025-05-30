module Provider.Executor (executeCmdIO, executeCmdsIO) where

import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Functor ((<&>))
import Data.List (intercalate)
import Interface
import Provider.Utils (join, try)
import System.Process (readCreateProcess, readProcess, shell)
import Types

executeCmdIO :: (HasLogger m, MonadIO m, MonadCatch m) => Cmd -> Stdin -> m (Either AppError Stdout)
executeCmdIO (Cmd cmd) (Stdin stdin) = do
  logInfo $ "execute command: " <> join " " cmd
  let filePath = concat $ take 1 cmd
  let opt = drop 1 cmd
  try (liftIO $ readProcess filePath opt stdin) <&> fmap Stdout

executeCmdsIO :: (MonadIO m, HasLogger m, MonadCatch m) => [Cmd] -> Stdin -> m (Either AppError Stdout)
executeCmdsIO [] _ = error "no command found"
executeCmdsIO [cmd] stdin = executeCmdIO cmd stdin
executeCmdsIO cmds (Stdin stdin) = liftIO $ do
  let cmdStr = intercalate " | " . map (\(Cmd parts) -> unwords parts) $ cmds
  try (liftIO $ readCreateProcess (shell cmdStr) stdin) <&> fmap Stdout