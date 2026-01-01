module Main (main) where

import Cli (process)
import Di ()
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import Types (AppError (..))

main :: IO ()
main = do
  getArgs >>= process >>= showErr >>= exit

showErr :: Either AppError () -> IO (Either () ())
showErr result = do
  case result of
    Left err -> do
      hPutStrLn stderr "[Error]"
      hPutStrLn stderr (formatError err)
      return $ Left ()
    Right () -> return $ Right ()

formatError :: AppError -> String
formatError (InvalidContestId msg) = "Invalid contest ID: " <> msg
formatError (InvalidProblemId msg) = "Invalid problem ID: " <> msg
formatError (InvalidSession msg) = "Invalid session: " <> msg
formatError SessionNotFound = "Session not found"
formatError (InvalidArgument msg) = msg
formatError (ProviderError msg) = msg

exit :: Either () () -> IO ()
exit result = do
  case result of
    Left _ -> exitFailure
    Right _ -> exitSuccess
