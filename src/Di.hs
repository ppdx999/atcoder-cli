-- src/App/Setup.hs
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Di () where

import Interface
import Provider.Atcoder (fetchProblemHtmlIO, fetchProblemIdsIO, fetchTestCasesIO, submitPageUrlIO, verifySessionIO)
import Provider.Browser (openBrowserIO)
import Provider.Clipboard (setClipboardIO)
import Provider.Config (loadSessionPathIO, loadTaskIO, loadTestDirIO)
import Provider.Executor (executeCmdIO, executeCmdsIO)
import Provider.FileSystem (createDirectoryIO, createDirectoryIfMissingIO, doesFileExistIO, getCurrentDirectoryIO, readDirIO, readFileIO, removeFileIO, saveFileIO)
import Provider.Language (buildLanguageIO, cleanupBuiltFileIO, detectLanguageIO, runTestCaseIO, toLanguageIO)
import Provider.Logger (logErrorIO, logInfoIO)
import Provider.Problem (htmlToTerminalIO, loadProblemCachePathIO, loadProblemHtmlIO, saveProblemHtmlIO)
import Provider.Os (detectOsIO)
import Provider.Req (getHtmlIO, reqGetIO, reqGetWithSessionIO)
import Provider.Session (loadSessionIO, saveSessionIO)
import Provider.Stdin (readLineIO)
import Provider.TestCase (loadTestCasesIO, reportTestResultIO, saveTestCaseIO)
import Provider.User (sendMsgIO)

instance HasLogger IO where
  logInfo = logInfoIO
  logError = logErrorIO

instance HasFileSystem IO where
  createDirectory = createDirectoryIO
  createDirectoryIfMissing = createDirectoryIfMissingIO
  getCurrentDirectory = getCurrentDirectoryIO
  readFile = readFileIO
  saveFile = saveFileIO
  removeFile = removeFileIO
  readDir = readDirIO
  doesFileExist = doesFileExistIO

instance HasConfig IO where
  loadTestDir = loadTestDirIO
  loadSessionPath = loadSessionPathIO
  loadTask = loadTaskIO

instance HasAtcoder IO where
  fetchProblemIds = fetchProblemIdsIO
  fetchTestCases = fetchTestCasesIO
  fetchProblemHtml = fetchProblemHtmlIO
  verifySession = verifySessionIO
  submitPageUrl = submitPageUrlIO

instance HasTestCase IO where
  loadTestCases = loadTestCasesIO
  saveTestCase = saveTestCaseIO
  reportTestResult = reportTestResultIO

instance HasSession IO where
  loadSession = loadSessionIO
  saveSession = saveSessionIO

instance HasLanguage IO where
  detectLanguage = detectLanguageIO
  toLanguage = toLanguageIO
  buildLanguage = buildLanguageIO
  runTestCase = runTestCaseIO
  cleanupBuiltFile = cleanupBuiltFileIO

instance HasExecutor IO where
  executeCmd = executeCmdIO
  executeCmds = executeCmdsIO

instance MonadReq IO where
  reqGet = reqGetIO
  reqGetWithSession = reqGetWithSessionIO
  getHtml = getHtmlIO

instance HasOs IO where
  detectOs = detectOsIO

instance HasStdin IO where
  readLine = readLineIO

instance HasUser IO where
  sendMsg = sendMsgIO

instance HasClipboard IO where
  setClipboard = setClipboardIO

instance HasBrowser IO where
  openBrowser = openBrowserIO

instance HasProblem IO where
  loadProblemCachePath = loadProblemCachePathIO
  loadProblemHtml = loadProblemHtmlIO
  saveProblemHtml = saveProblemHtmlIO
  htmlToTerminal = htmlToTerminalIO