-- src/App/Setup.hs
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- Orphan instance warning を抑制
{-# OPTIONS_GHC -Wno-orphans #-}

module Di (runAppM) where

import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Provider.Atcoder (fetchProblemIdsIO)
import Provider.FileSystem (createDirectoryIO)
import Provider.Logger (logErrorIO, logInfoIO)
import Types (AppError)
import Usecase.Deps

instance HasLogger IO where
  logInfo = logInfoIO
  logError = logErrorIO

instance HasFileSystem IO where
  createDirectory = createDirectoryIO

instance HasAtcoder IO where
  fetchProblemIds = fetchProblemIdsIO

runAppM :: ExceptT AppError IO a -> IO (Either AppError a)
runAppM = runExceptT
