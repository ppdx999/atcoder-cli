module Cli (process) where

import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Interface
import Types
import Usecase.Download (download)
import Usecase.Init (initContest)
import Usecase.Login (login)
import Usecase.Problem (problem)
import Usecase.Submit (submit)
import Usecase.Test (test)

process ::
  ( MonadIO m,
    HasAtcoder m,
    HasLogger m,
    HasLanguage m,
    HasFileSystem m,
    HasConfig m,
    HasTestCase m,
    HasStdin m,
    HasSession m,
    HasClipboard m,
    HasUser m,
    HasBrowser m,
    HasProblem m
  ) =>
  [String] ->
  m (Either AppError ())
process ["--help"] =
  return $
    Left $
      InvalidArgument $
        unlines
          [ "Invalid arguments: Usage:",
            "atcli <command>",
            "",
            "command:",
            "  init <contestId>",
            "  download",
            "  login",
            "  test",
            "  submit",
            "  problem",
            ""
          ]
process ["-h"] = process ["--help"]
process ["init", cid] = runInit cid
process ["i", cid] = runInit cid
process ["download"] = download
process ["d"] = download
process ["login"] = login
process ["l"] = login
process ("test" : args) = runTest args
process ("t" : args) = runTest args
process ["submit"] = submit
process ["s"] = submit
process ["problem"] = problem
process ["p"] = problem
process _ = process ["--help"]

runInit ::
  ( MonadIO m,
    HasAtcoder m,
    HasLogger m,
    HasFileSystem m
  ) =>
  String ->
  m (Either AppError ())
runInit cid =
  either (return . Left) initContest (validateContestId cid)

runTest ::
  ( HasLogger m,
    HasLanguage m,
    HasTestCase m
  ) =>
  [String] ->
  m (Either AppError ())
runTest [] = runExceptT $ do
  ExceptT detectLanguage
    >>= ExceptT . test
runTest ["-l", lang] = runExceptT $ do
  ExceptT (toLanguage lang)
    >>= ExceptT . test
runTest _ =
  return $
    Left $
      InvalidArgument $
        unlines
          [ ""
          ]
