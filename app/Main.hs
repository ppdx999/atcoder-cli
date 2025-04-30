{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.Except (liftEither)
import Control.Monad.Trans.Except (ExceptT, throwE)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Di (runAppM)
import Domain.Types (DomainError (InfraError))
import Domain.Validate (mkContestId)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (stderr)
import Usecase.Init (initContest)

main :: IO ()
main = do
  getArgs >>= runAppM . runMain . textize >>= showErr >>= exit

textize :: [String] -> [T.Text]
textize = map T.pack

runMain :: [T.Text] -> ExceptT DomainError IO ()
runMain ["init", contestIdStr] = do
  contestId <- liftEither $ mkContestId contestIdStr
  initContest contestId
runMain _ =
  throwE $ InfraError "Invalid arguments: Usage: atcli <contest-id>"

showErr :: Either DomainError () -> IO (Either () ())
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