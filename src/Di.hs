-- src/App/Setup.hs
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- Orphan instance warning を抑制
{-# OPTIONS_GHC -Wno-orphans #-}

module Di (runAppM) where

import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Interface
import Provider.Atcoder (AtCoderEnv (AtCoderEnv), fetchProblemIdsIO, fetchTestCasesIO, verifySessionIO)
import Provider.FileSystem (createDirectoryIO, getCurrentDirectoryIO, readFileIO, saveFileIO)
import Provider.Logger (logErrorIO, logInfoIO)
import Provider.Req (getHtmlIO, reqGetIO)
import Provider.Session (loadSessionIO, saveSessionIO)
import Provider.Stdin (readLineIO)
import Types (AppError)

instance HasLogger IO where
  logInfo = logInfoIO
  logError = logErrorIO

instance HasFileSystem IO where
  createDirectory :: FilePath -> IO (Either AppError ())
  createDirectory = createDirectoryIO
  getCurrentDirectory = getCurrentDirectoryIO
  readFile = readFileIO
  saveFile = saveFileIO

instance HasAtcoder IO where
  fetchProblemIds = fetchProblemIdsIO AtCoderEnv
  fetchTestCases = fetchTestCasesIO AtCoderEnv
  verifySession = verifySessionIO AtCoderEnv

instance HasSession IO where
  loadSession = loadSessionIO
  saveSession = saveSessionIO

instance MonadReq IO where
  reqGet = reqGetIO
  reqGetWithSession = reqGetWithSession
  getHtml = getHtmlIO

instance HasStdin IO where
  readLine = readLineIO

runAppM :: ExceptT AppError IO a -> IO (Either AppError a)
runAppM = runExceptT
