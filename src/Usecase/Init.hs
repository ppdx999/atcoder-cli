{-# LANGUAGE OverloadedStrings #-}

module Usecase.Init
  ( initContest,
  )
where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (ExceptT))
import Data.Foldable (traverse_)
import qualified Data.Text as T
import Interface
import System.FilePath ((</>))
import Types

initContest ::
  ( HasAtcoder m,
    HasFileSystem m,
    HasLogger m
  ) =>
  ContestId ->
  ExceptT AppError m ()
initContest contestId = do
  let contestName = deContestId contestId
  lift $ logInfo $ "Initializing contest: " <> contestName

  let contestDir = T.unpack contestName
  lift $ logInfo $ "Creating directory: " <> T.pack contestDir
  ExceptT $ createDirectory contestDir

  lift $ logInfo "Fetching problem list..."
  problemIds <- ExceptT $ fetchProblemIds contestId

  lift $ logInfo $ "Found " <> T.pack (show $ length problemIds) <> " problems."

  let createProblemDir problemId = do
        let problemDir = contestDir </> T.unpack (deProblemId problemId)
        lift $ logInfo $ "Creating directory: " <> T.pack problemDir
        ExceptT $ createDirectory problemDir

  traverse_ createProblemDir problemIds