{-# LANGUAGE OverloadedStrings #-}

module Usecase.Init
  ( initContest,
  )
where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (ExceptT))
import Data.Foldable (traverse_)
import qualified Data.Text as T
import Domain.Types
import System.FilePath ((</>))
import Usecase.Ports

initContest ::
  ( HasFetchProblemList m,
    HasCreateDirectory m,
    HasLogger m
  ) =>
  ContestId ->
  ExceptT DomainError m ()
initContest contestId@(ContestId contestName) = do
  lift $ logInfo $ "Initializing contest: " <> contestName

  let contestDir = T.unpack contestName
  lift $ logInfo $ "Creating directory: " <> T.pack contestDir
  ExceptT $ createDirectory contestDir

  lift $ logInfo "Fetching problem list..."
  problemIds <- ExceptT $ fetchProblemList contestId

  lift $ logInfo $ "Found " <> T.pack (show $ length problemIds) <> " problems."

  let createProblemDir (ProblemId problemName) = do
        let problemDir = contestDir </> T.unpack problemName
        lift $ logInfo $ "Creating directory: " <> T.pack problemDir
        ExceptT $ createDirectory problemDir -- 失敗したら ExceptT が Left を伝播

