{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Interface
  ( HasAtcoder (..),
    HasFileSystem (..),
    HasConfig (..),
    HasLogger (..),
    MonadReq (..),
    HasStdin (..),
    HasSession (..),
    HasTestCase (..),
    HasLanguage (..),
    HasExecutor (..),
    HasUser (..),
    HasClipboard (..),
    HasBrowser (..),
    HasOs (..),
  )
where

import Control.Monad.Catch (MonadThrow)
import Data.Proxy (Proxy)
import Network.HTTP.Req
  ( HttpConfig,
    HttpResponse,
    Option,
    Scheme (Https),
  )
import Text.URI (URI)
import Types

---------------------------------------------
-- Infra
---------------------------------------------

class (Monad m) => HasLogger m where
  logInfo :: String -> m ()
  logError :: String -> m ()

class (Monad m, MonadThrow m) => HasFileSystem m where
  createDirectory :: FilePath -> m (Either AppError ())
  createDirectoryIfMissing :: Bool -> FilePath -> m (Either AppError ())
  getCurrentDirectory :: m FilePath
  readFile :: FilePath -> m (Either AppError String)
  saveFile :: FilePath -> String -> m (Either AppError ())
  readDir :: FilePath -> m (Either AppError [FilePath])
  doesFileExist :: FilePath -> m Bool
  removeFile :: FilePath -> m (Either AppError ())

class (Monad m) => HasExecutor m where
  executeCmd :: Cmd -> Stdin -> m (Either AppError Stdout)
  executeCmds :: [Cmd] -> Stdin -> m (Either AppError Stdout)

class (Monad m) => MonadReq m where
  reqGet ::
    (HttpResponse r) =>
    String ->
    Proxy r ->
    HttpConfig ->
    Option Https ->
    m (Either AppError r)
  reqGetWithSession ::
    (HttpResponse r) =>
    Session ->
    String ->
    Proxy r ->
    HttpConfig ->
    Option Https ->
    m (Either AppError r)
  getHtml :: String -> m (Either AppError String)

class (Monad m) => HasStdin m where
  readLine :: m String

class (Monad m) => HasOs m where
  detectOs :: m OS

class (Monad m) => HasClipboard m where
  setClipboard :: String -> m (Either AppError ())

class (Monad m) => HasBrowser m where
  openBrowser :: URI -> m (Either AppError ())

---------------------------------------------
-- Service
---------------------------------------------

class (Monad m) => HasConfig m where
  loadTestDir :: m (Either AppError FilePath)
  loadSessionPath :: m (Either AppError FilePath)
  loadTask :: m (Either AppError Task)

class (Monad m) => HasSession m where
  loadSession :: m (Either AppError Session)
  saveSession :: Session -> m (Either AppError ())

class (Monad m) => HasTestCase m where
  loadTestCases :: m (Either AppError [TestCase])
  saveTestCase :: TestCase -> m (Either AppError ())
  reportTestResult :: (TestCase, RunTestCaseResult) -> m (Either AppError ())

class (Monad m) => HasLanguage m where
  detectLanguage :: m (Either AppError Language)
  toLanguage :: String -> m (Either AppError Language)
  buildLanguage :: Language -> m (Either AppError ())
  runTestCase :: Language -> TestCase -> m (Either AppError RunTestCaseResult)
  cleanupBuiltFile :: Language -> m (Either AppError ())

class (Monad m) => HasAtcoder m where
  fetchProblemIds :: ContestId -> m (Either AppError [ProblemId])
  fetchTestCases :: Task -> m (Either AppError [TestCase])
  verifySession :: Session -> m (Either AppError Bool)
  submitPageUrl :: Task -> m URI

class (Monad m) => HasUser m where
  sendMsg :: String -> m ()
