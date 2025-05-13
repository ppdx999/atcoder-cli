module Cli (process) where

import Control.Monad.IO.Class (MonadIO)
import Interface
import Types
import Usecase.Download (download)
import Usecase.Init (initContest)
import Usecase.Login (login)
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
    HasBrowser m
  ) =>
  [String] ->
  m (Either AppError ())
process ["init", cid] = runInit cid
process ["i", cid] = runInit cid
process ["download"] = download
process ["d"] = download
process ["login"] = login
process ["l"] = login
process ["test"] = test
process ["t"] = test
process ["submit"] = submit
process ["s"] = submit
process _ =
  return $
    Left $
      ProviderError $
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
            ""
          ]

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