{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.Except (ExceptT (ExceptT), liftEither)
import Control.Monad.Trans.Except (throwE)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Di (runAppM)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (stderr)
import Types (AppError (ProviderError), validateContestId)
import Usecase.Download (download)
import Usecase.Init (initContest)
import Usecase.Login (login)
import Usecase.Test (test)

main :: IO ()
main = do
  getArgs >>= runAppM . runMain . textize >>= showErr >>= exit

textize :: [String] -> [T.Text]
textize = map T.pack

runMain :: [T.Text] -> ExceptT AppError IO ()
runMain ["init", contestIdStr] = do
  contestId <- liftEither $ validateContestId contestIdStr
  initContest contestId
runMain ["download"] = download
runMain ["login"] = login
runMain ["test"] = ExceptT test
runMain _ =
  throwE $ ProviderError "Invalid arguments: Usage: atcli <contest-id>"

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