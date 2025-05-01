-- src/App/Setup.hs
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- Orphan instance warning を抑制
{-# OPTIONS_GHC -Wno-orphans #-}

module Di (runAppM) where

import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Provider.Atcoder (fetchProblemIdsIO, fetchTestCasesIO)
import Provider.FileSystem (createDirectoryIO, getCurrentDirectoryIO, saveFileIO)
import Provider.Logger (logErrorIO, logInfoIO)
import Types (AppError)
import Usecase.Deps

instance HasLogger IO where
  logInfo = logInfoIO
  logError = logErrorIO

instance HasFileSystem IO where
  createDirectory :: FilePath -> IO (Either AppError ())
  createDirectory = createDirectoryIO
  getCurrentDirectory = getCurrentDirectoryIO
  saveFile = saveFileIO

instance HasAtcoder IO where
  fetchProblemIds = fetchProblemIdsIO
  fetchTestCases = fetchTestCasesIO

runAppM :: ExceptT AppError IO a -> IO (Either AppError a)
runAppM = runExceptT
