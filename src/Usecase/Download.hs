-- src/Usecase/Download.hs
{-# LANGUAGE OverloadedStrings #-}

module Usecase.Download
  ( download,
  )
where

import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Except (ExceptT (..))
import Data.Foldable (traverse_)
import qualified Data.Text as T
import Interface
import Types

download ::
  ( HasLogger m,
    HasAtcoder m,
    HasConfig m,
    HasTestCase m
  ) =>
  ExceptT AppError m ()
download = do
  logInfoE "Starting download..."

  task@(Task (ContestId cid) (ProblemId pid)) <- ExceptT loadTask
  logInfoE $ "Target: Contest=" <> cid <> ", Problem=" <> pid

  logInfoE "Fetching test cases..."
  testCases <- ExceptT $ fetchTestCases task
  logInfoE $ "Found " <> T.pack (show $ length testCases) <> " test cases."

  traverse_ (ExceptT . saveTestCase) testCases
  where
    logInfoE :: (HasLogger m, MonadTrans t) => T.Text -> t m ()
    logInfoE = lift . logInfo
