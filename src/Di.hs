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
import Provider.Config (loadSessionPathIO, loadTaskIO, loadTestDirIO)
import Provider.FileSystem (createDirectoryIO, createDirectoryIfMissingIO, doesFileExistIO, getCurrentDirectoryIO, readDirIO, readFileIO, saveFileIO)
import Provider.Language (detectLanguageIO)
import Provider.Logger (logErrorIO, logInfoIO)
import Provider.Req (getHtmlIO, reqGetIO, reqGetWithSessionIO)
import Provider.Session (loadSessionIO, saveSessionIO)
import Provider.Stdin (readLineIO)
import Provider.TestCase (saveTestCaseIO)
import Provider.User (sendMsgIO)
import Types (AppError)

instance HasLogger IO where
  logInfo = logInfoIO
  logError = logErrorIO

instance HasFileSystem IO where
  createDirectory = createDirectoryIO
  createDirectoryIfMissing = createDirectoryIfMissingIO
  getCurrentDirectory = getCurrentDirectoryIO
  readFile = readFileIO
  saveFile = saveFileIO
  readDir = readDirIO
  doesFileExist = doesFileExistIO

instance HasConfig IO where
  loadTestDir = loadTestDirIO
  loadSessionPath = loadSessionPathIO
  loadTask = loadTaskIO

instance HasAtcoder IO where
  fetchProblemIds = fetchProblemIdsIO AtCoderEnv
  fetchTestCases = fetchTestCasesIO AtCoderEnv
  verifySession = verifySessionIO AtCoderEnv

instance HasTestCase IO where
  saveTestCase = saveTestCaseIO

instance HasSession IO where
  loadSession = loadSessionIO
  saveSession = saveSessionIO

instance HasLanguage IO where
  detectLanguage = detectLanguageIO

instance MonadReq IO where
  reqGet = reqGetIO
  reqGetWithSession = reqGetWithSessionIO
  getHtml = getHtmlIO

instance HasStdin IO where
  readLine = readLineIO

instance HasUser IO where
  sendMsg = sendMsgIO

runAppM :: ExceptT AppError IO a -> IO (Either AppError a)
runAppM = runExceptT
