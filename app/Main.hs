module Main (main) where

import Cli (process)
import Di ()
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPrint, hPutStrLn, stderr)
import Types (AppError)

main :: IO ()
main = do
  getArgs >>= process >>= showErr >>= exit

showErr :: Either AppError () -> IO (Either () ())
showErr result = do
  case result of
    Left err -> do
      hPutStrLn stderr "[Error]"
      hPrint stderr err
      return $ Left ()
    Right () -> return $ Right ()

exit :: Either () () -> IO ()
exit result = do
  case result of
    Left _ -> exitFailure
    Right _ -> exitSuccess