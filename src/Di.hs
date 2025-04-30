-- src/App/Setup.hs
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- Orphan instance warning を抑制
{-# OPTIONS_GHC -Wno-orphans #-}

module Di (runAppM) where

import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Domain.Types (DomainError)
import Infra.Atcoder (fetchProblemIdsIO)
import Infra.FileSystem (createDirectoryIO)
import Infra.Logger (logErrorIO, logInfoIO)
import Usecase.Ports

instance HasLogger IO where
  logInfo = logInfoIO
  logError = logErrorIO

instance HasCreateDirectory IO where
  createDirectory = createDirectoryIO

instance HasFetchProblemIds IO where
  fetchProblemIds = fetchProblemIdsIO

runAppM :: ExceptT DomainError IO a -> IO (Either DomainError a)
runAppM = runExceptT
