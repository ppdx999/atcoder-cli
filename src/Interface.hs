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
    HasReporter (..),
    HasUser (..),
  )
where

import Control.Monad.Catch (MonadThrow)
import Data.ByteString (ByteString)
import Data.Proxy (Proxy)
import Data.Text (Text)
import Network.HTTP.Req
  ( HttpConfig,
    HttpResponse,
    Option,
    Scheme (Https),
  )
import Types

class (Monad m) => HasLogger m where
  logInfo :: Text -> m ()
  logError :: Text -> m ()

class (Monad m, MonadThrow m) => HasFileSystem m where
  createDirectory :: FilePath -> m (Either AppError ())
  createDirectoryIfMissing :: Bool -> FilePath -> m (Either AppError ())
  getCurrentDirectory :: m FilePath
  readFile :: FilePath -> m (Either AppError ByteString)
  saveFile :: FilePath -> ByteString -> m (Either AppError ())
  readDir :: FilePath -> m (Either AppError [FilePath])
  doesFileExist :: FilePath -> m Bool

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

class (Monad m) => HasLanguage m where
  detectLanguage :: m (Either AppError Language)
  runTestCase :: Language -> TestCase -> m (Either AppError RunTestCaseResult)

class (Monad m) => HasExecutor m where
  executeCmd :: Cmd -> Stdin -> m (Either AppError Stdout)

class (Monad m) => HasReporter m where
  report :: TestCase -> RunTestCaseResult -> m (Either AppError ())

class (Monad m) => HasAtcoder m where
  fetchProblemIds :: ContestId -> m (Either AppError [ProblemId])
  fetchTestCases :: Task -> m (Either AppError [TestCase])
  verifySession :: Session -> m (Either AppError Bool)

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
  getHtml :: String -> m (Either AppError Text)

class (Monad m) => HasStdin m where
  readLine :: m Text

class (Monad m) => HasUser m where
  sendMsg :: Text -> m ()
