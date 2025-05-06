-- test/Mock.hs
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Data.ByteString (ByteString)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text.Encoding as TEnc
import Interface
import Types

data MockState = MockState
  { msLogs :: [Text],
    msCreatedDirs :: Set FilePath,
    msProblemIdsResult :: Either AppError [ProblemId],
    msCreateDirResult :: FilePath -> Either AppError (),
    msCurrentDir :: FilePath,
    msTestCasesResult :: Either AppError [TestCase],
    msSaveFileResult :: FilePath -> ByteString -> Either AppError (),
    msSavedFiles :: Map FilePath ByteString,
    msStdinQueue :: [Text],
    msConfig :: Config,
    msLoadSessionResult :: FilePath -> Either AppError Session,
    msSaveSessionResult :: FilePath -> Session -> Either AppError (),
    msSavedSessions :: [Session],
    msVerifySessionResultsQueue :: [Either AppError Bool]
  }

initialMockState :: MockState
initialMockState =
  MockState
    { msLogs = [],
      msCreatedDirs = Set.empty,
      msProblemIdsResult = Right [],
      msCreateDirResult = \_ -> Right (),
      msCurrentDir = "/tmp/abc100/a",
      msTestCasesResult = Right [],
      msSaveFileResult = \_ _ -> Right (),
      msSavedFiles = Map.empty,
      msStdinQueue = [],
      msConfig = Config {sessionPath = "/mock/session.txt"},
      msLoadSessionResult = \_ -> Left SessionNotFound,
      msSaveSessionResult = \_ _ -> Right (),
      msSavedSessions = [],
      msVerifySessionResultsQueue = [Right False]
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

instance HasLogger MockApp where
  logInfo msg = modify $ \s -> s {msLogs = msLogs s ++ [msg]}
  logError msg = modify $ \s -> s {msLogs = msLogs s ++ ["[ERROR] " <> msg]}

instance HasFileSystem MockApp where
  createDirectory path = do
    resultFunc <- gets msCreateDirResult
    case resultFunc path of
      Right () -> do
        modify $ \s -> s {msCreatedDirs = Set.insert path (msCreatedDirs s)}
        pure $ Right ()
      Left err -> pure $ Left err
  getCurrentDirectory = gets msCurrentDir
  saveFile path content = do
    resultFunc <- gets msSaveFileResult
    case resultFunc path content of
      Right () -> do
        modify $ \s -> s {msSavedFiles = Map.insert path content (msSavedFiles s)}
        pure $ Right ()
      Left err -> pure $ Left err
  loadSession path = do
    resultFunc <- gets msLoadSessionResult
    pure $ resultFunc path
  saveSession path session = do
    resultFunc <- gets msSaveSessionResult
    case resultFunc path session of
      Right () -> do
        modify $ \s -> s {msSavedFiles = Map.insert path (TEnc.encodeUtf8 (case session of (Session s') -> s')) (msSavedFiles s)}
        modify $ \s -> s {msSavedSessions = msSavedSessions s ++ [session]}
        pure $ Right ()
      Left err -> pure $ Left err

instance HasAtcoder MockApp where
  fetchProblemIds _contestId = gets msProblemIdsResult
  fetchTestCases _task = gets msTestCasesResult
  verifySession _session = do
    queue <- gets msVerifySessionResultsQueue
    case queue of
      [] -> error "MockApp: verifySession called but no results in queue"
      (r : rs) -> do
        modify $ \s -> s {msVerifySessionResultsQueue = rs}
        pure r

instance HasStdin MockApp where
  readLine = do
    queue <- gets msStdinQueue
    case queue of
      [] -> error "MockApp: readLine called on empty stdin queue"
      (x : xs) -> do
        modify $ \s -> s {msStdinQueue = xs}
        pure x

instance HasConfig MockApp where
  getConfig = gets msConfig
