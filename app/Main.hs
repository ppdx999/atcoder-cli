{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Di ()
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (stderr)
import Types (AppError (ProviderError), validateContestId)
import Usecase.Download (download)
import Usecase.Init (initContest)
import Usecase.Login (login)
import Usecase.Submit (submit)
import Usecase.Test (test)

main :: IO ()
main = do
  getArgs >>= runMain . textize >>= showErr >>= exit

textize :: [String] -> [T.Text]
textize = map T.pack

runMain :: [T.Text] -> IO (Either AppError ())
runMain ["init", contestIdStr] =
  either (return . Left) initContest (validateContestId contestIdStr)
runMain ["download"] = download
runMain ["login"] = login
runMain ["test"] = test
runMain ["submit"] = submit
runMain _ = return $ Left $ ProviderError "Invalid arguments: Usage: atcli <contest-id>"

showErr :: Either AppError () -> IO (Either () ())
showErr result = do
  case result of
    Left err -> do
      TIO.hPutStrLn stderr "[Error]"
      TIO.hPutStrLn stderr $ T.pack (show err)
      return $ Left ()
    Right () -> return $ Right ()

exit :: Either () () -> IO ()
exit result = do
  case result of
    Left _ -> exitFailure
    Right _ -> exitSuccess