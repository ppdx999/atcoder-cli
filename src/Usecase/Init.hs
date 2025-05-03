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
initContest contestId@(ContestId contestIdText) = do
  lift $ logInfo $ "Initializing contest: " <> contestIdText

  lift $ logInfo $ "Creating directory: " <> contestIdText
  ExceptT $ createDirectory $ T.unpack contestIdText

  lift $ logInfo "Fetching problem list..."
  problemIds <- ExceptT $ fetchProblemIds contestId

  lift $ logInfo $ "Found " <> T.pack (show $ length problemIds) <> " problems."

  let createProblemDir (ProblemId problemIdText) = do
        let problemDir = T.unpack contestIdText </> T.unpack problemIdText
        lift $ logInfo $ "Creating directory: " <> T.pack problemDir
        ExceptT $ createDirectory problemDir

  traverse_ createProblemDir problemIds