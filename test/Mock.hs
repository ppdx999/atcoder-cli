-- test/Mock.hs
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Mock
  ( MockState (..),
    initialMockState,
    MockApp (..),
    execMockApp,
  )
where

import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.State.Strict (MonadState, StateT (..), gets, modify, runStateT)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import Interface
import Text.URI (URI, mkURI)
import Types

data MockState = MockState
  { -- Mock HasFileSystem--------------------------
    msCreatedDirs :: Set FilePath, -- createDirectory and createDirectoryIfMissing
    msGetCurrentDirectory :: FilePath,
    msReadFile :: Either AppError String,
    msSaveFile :: Map FilePath String,
    msReadDir :: Either AppError [FilePath],
    msDoesFileExist :: [Bool],
    msRemoveFile :: Either AppError (),
    -----------------------------------------------

    -- Mock HasExecutor ---------------------------
    msExecuteCmd :: Either AppError Stdout,
    -----------------------------------------------

    -- Mock MonadReq ---------------------------
    msGetHtml :: Either AppError String,
    -----------------------------------------------

    -- Mock HasStdin ---------------------------
    msReadLine :: [String],
    -----------------------------------------------

    -- Mock HasOs ------------------------------
    msDetectOs :: OS,
    -----------------------------------------------

    -- Mock HasClipboard -----------------------
    msSetClipboard :: [String],
    -----------------------------------------------

    -- Mock HasBrowser -------------------------
    msOpenBrowser :: [URI],
    -----------------------------------------------

    -- Mock HasConfig --------------------------
    msLoadTestDir :: Either AppError FilePath,
    msLoadSessionPath :: Either AppError FilePath,
    msLoadTask :: Either AppError Task,
    -----------------------------------------------

    -- Mock HasSession -------------------------
    msLoadSession :: Either AppError Session,
    msSaveSession :: [Session],
    -----------------------------------------------

    -- Mock HasTestCase ------------------------
    msLoadTestCases :: Either AppError [TestCase],
    msSaveTestCase :: [TestCase],
    -----------------------------------------------

    -- Mock HasLanguage ------------------------
    msDetectLanguage :: Either AppError Language,
    msBuildLanguage :: Either AppError (),
    msRunTestCase :: Either AppError RunTestCaseResult,
    msCleanupBuiltFile :: Either AppError (),
    -----------------------------------------------

    -- Mock HasAtcoder -------------------------
    msFetchProblemIds :: Either AppError [ProblemId],
    msFetchTestCases :: Either AppError [TestCase],
    msVerifySession :: [Either AppError Bool],
    msSubmitPageUrl :: String,
    -----------------------------------------------

    -- Mock HasUser--------------------------------
    msSendMsg :: [String]
    -----------------------------------------------
  }

initialMockState :: MockState
initialMockState =
  MockState
    { -- Mock HasFileSystem--------------------------
      msCreatedDirs = Set.empty,
      msGetCurrentDirectory = "",
      msReadFile = Left (ProviderError "uninitialized msReadFile"),
      msSaveFile = Map.empty,
      msReadDir = Left (ProviderError "uninitialized msReadDir"),
      msDoesFileExist = [],
      msRemoveFile = Left (ProviderError "uninitialized msRemoveFile"),
      -----------------------------------------------

      -- Mock HasExecutor ---------------------------
      msExecuteCmd = Left (ProviderError "uninitialized msExecuteCmd"),
      -----------------------------------------------

      -- Mock MonadReq ---------------------------
      msGetHtml = Left (ProviderError "uninitialized msGetHtml"),
      -----------------------------------------------

      -- Mock HasStdin ---------------------------
      msReadLine = [],
      -----------------------------------------------

      -- Mock HasOs ------------------------------
      msDetectOs = Linux, -- 適当な初期値（必要に応じて Windows 等に変更）
      -----------------------------------------------

      -- Mock HasClipboard -----------------------
      msSetClipboard = [],
      -----------------------------------------------

      -- Mock HasBrowser -------------------------
      msOpenBrowser = [],
      -----------------------------------------------

      -- Mock HasConfig --------------------------
      msLoadTestDir = Left (ProviderError "uninitialized msLoadTestDir"),
      msLoadSessionPath = Left (ProviderError "uninitialized msLoadSessionPath"),
      msLoadTask = Left (ProviderError "uninitialized msLoadTask"),
      -----------------------------------------------

      -- Mock HasSession -------------------------
      msLoadSession = Left (ProviderError "uninitialized msLoadSession"),
      msSaveSession = [],
      -----------------------------------------------

      -- Mock HasTestCase ------------------------
      msLoadTestCases = Left (ProviderError "uninitialized msLoadTestCases"),
      msSaveTestCase = [],
      -----------------------------------------------

      -- Mock HasLanguage ------------------------
      msDetectLanguage = Left (ProviderError "uninitialized msDetectLanguage"),
      msBuildLanguage = Left (ProviderError "uninitialized msBuildLanguage"),
      msRunTestCase = Left (ProviderError "uninitialized msRunTestCase"),
      msCleanupBuiltFile = Left (ProviderError "uninitialized msCleanupBuiltFile"),
      -----------------------------------------------

      -- Mock HasAtcoder -------------------------
      msFetchProblemIds = Left (ProviderError "uninitialized msFetchProblemIds"),
      msFetchTestCases = Left (ProviderError "uninitialized msFetchTestCases"),
      msVerifySession = [Left (ProviderError "uninitialized msVerifySession")],
      msSubmitPageUrl = "https://example.com",
      -----------------------------------------------

      -- Mock HasUser--------------------------------
      msSendMsg = []
      -----------------------------------------------
    }

newtype MockApp a = MockApp {runMockApp :: StateT MockState IO a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadState MockState,
      MonadThrow
    )

execMockApp :: MockApp a -> MockState -> IO (a, MockState)
execMockApp = runStateT . runMockApp

---------------------------------------------
-- Infra
---------------------------------------------
instance HasLogger MockApp where
  logInfo _msg = return ()
  logError _msg = return ()

instance HasFileSystem MockApp where
  createDirectory path = do
    modify $ \s -> s {msCreatedDirs = Set.insert path (msCreatedDirs s)}
    pure $ Right ()

  createDirectoryIfMissing _missing path = do
    modify $ \s -> s {msCreatedDirs = Set.insert path (msCreatedDirs s)}
    pure $ Right ()

  getCurrentDirectory = gets msGetCurrentDirectory
  readFile _filePath = gets msReadFile
  saveFile path content = do
    modify $ \s -> s {msSaveFile = Map.insert path content (msSaveFile s)}
    pure $ Right ()
  readDir _filePath = gets msReadDir
  doesFileExist _path = do
    queue <- gets msDoesFileExist
    case queue of
      [] -> error "MockApp: doesFileExist called on empty queue"
      (x : xs) -> do
        modify $ \s -> s {msDoesFileExist = xs}
        pure x

instance MonadReq MockApp where
  getHtml _url = gets msGetHtml

instance HasStdin MockApp where
  readLine = do
    queue <- gets msReadLine
    case queue of
      [] -> error "MockApp: readLine called on empty stdin queue"
      (x : xs) -> do
        modify $ \s -> s {msReadLine = xs}
        pure x

instance HasOs MockApp where
  detectOs = gets msDetectOs

instance HasClipboard MockApp where
  setClipboard content = do
    modify $ \s -> s {msSetClipboard = msSetClipboard s ++ [content]}
    pure $ Right ()

instance HasBrowser MockApp where
  openBrowser uri = do
    modify $ \s -> s {msOpenBrowser = uri : msOpenBrowser s}
    pure $ Right ()

---------------------------------------------
-- Service
---------------------------------------------

instance HasConfig MockApp where
  loadSessionPath = gets msLoadSessionPath
  loadTestDir = gets msLoadTestDir
  loadTask = gets msLoadTask

instance HasSession MockApp where
  loadSession = gets msLoadSession
  saveSession session = do
    modify $ \s -> s {msSaveSession = msSaveSession s ++ [session]}
    pure $ Right ()

instance HasTestCase MockApp where
  saveTestCase tc = do
    modify $ \s -> s {msSaveTestCase = msSaveTestCase s ++ [tc]}
    pure $ Right ()

instance HasLanguage MockApp where
  detectLanguage = gets msDetectLanguage
  buildLanguage _language = gets msBuildLanguage
  runTestCase _language _tcs = gets msRunTestCase
  cleanupBuiltFile _language = gets msCleanupBuiltFile

instance HasAtcoder MockApp where
  fetchProblemIds _contestId = gets msFetchProblemIds
  fetchTestCases _task = gets msFetchTestCases
  verifySession _session = do
    queue <- gets msVerifySession
    case queue of
      [] -> error "MockApp: verifySession called on empty queue"
      (x : xs) -> do
        modify $ \s -> s {msVerifySession = xs}
        pure x
  submitPageUrl _task = do
    url <- gets msSubmitPageUrl
    mkURI (T.pack url)

instance HasUser MockApp where
  sendMsg msg = modify $ \s -> s {msSendMsg = msSendMsg s ++ [msg]}