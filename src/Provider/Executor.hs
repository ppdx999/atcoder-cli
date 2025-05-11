module Provider.Executor (executeCmdIO) where

import Provider.Utils (try)
import System.Process (readProcess)
import Types

executeCmdIO :: Cmd -> Stdin -> IO (Either AppError Stdout)
executeCmdIO (Cmd cmd) (Stdin stdin) =
  try (readProcess (concat $ take 1 cmd) (drop 1 cmd) stdin)
    >>= either (return . Left) (return . Right . Stdout)
