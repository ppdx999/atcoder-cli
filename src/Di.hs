-- src/App/Setup.hs
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- Orphan instance warning を抑制
{-# OPTIONS_GHC -Wno-orphans #-}

module Di (runAppM) where

import Control.Monad.Trans.Except (ExceptT, runExceptT)
import qualified Data.ByteString.Char8 as BSC
import Interface
import Provider.Atcoder (AtCoderEnv (AtCoderEnv), fetchProblemIdsIO, fetchTestCasesIO)
import Provider.FileSystem (createDirectoryIO, getCurrentDirectoryIO, saveFileIO)
import Provider.Logger (logErrorIO, logInfoIO)
import Provider.Req (reqGetIO)
import Types (AppError)

instance HasLogger IO where
  logInfo = logInfoIO
  logError = logErrorIO

instance HasFileSystem IO where
  createDirectory :: FilePath -> IO (Either AppError ())
  createDirectory = createDirectoryIO
  getCurrentDirectory = getCurrentDirectoryIO
  saveFile = saveFileIO

instance HasAtcoder IO where
  fetchProblemIds = fetchProblemIdsIO AtCoderEnv
  fetchTestCases = fetchTestCasesIO AtCoderEnv

instance MonadReq IO where
  reqGet :: String -> IO (Either AppError BSC.ByteString)
  reqGet = reqGetIO

runAppM :: ExceptT AppError IO a -> IO (Either AppError a)
runAppM = runExceptT
