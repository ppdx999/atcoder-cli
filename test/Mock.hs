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
import Interface
import Types

data MockState = MockState
  { msLogs :: [Text],
    msMsgs :: [Text],
    msCreatedDirs :: Set FilePath,
    msProblemIdsResult :: Either AppError [ProblemId],
    msCreateDirResult :: FilePath -> Either AppError (),
    msCreateDirIfMissing :: Bool -> FilePath -> Either AppError (),
    msDoesFileExistQueue :: [Bool],
    msCurrentDir :: FilePath,
    msTestCasesResult :: Either AppError [TestCase],
    msReadFiles :: Either AppError ByteString,
    msSaveFileResult :: FilePath -> ByteString -> Either AppError (),
    msSavedFiles :: Map FilePath ByteString,
    msStdinQueue :: [Text],
    msSessionPath :: Either AppError FilePath,
    msTask :: Either AppError Task,
    msLoadSessionResult :: Either AppError Session,
    msSaveSessionResult :: Session -> Either AppError (),
    msSavedSessions :: [Session],
    msVerifySessionResultsQueue :: [Either AppError Bool]
  }

initialMockState :: MockState
initialMockState =
  MockState
    { msLogs = [],
      msMsgs = [],
      msCreatedDirs = Set.empty,
      msProblemIdsResult = Right [],
      msCreateDirResult = \_ -> Right (),
      msCreateDirIfMissing = \_ _ -> Right (),
      msDoesFileExistQueue = [True],
      msCurrentDir = "/tmp/abc100/a",
      msTestCasesResult = Right [],
      msReadFiles = Right "fileData",
      msSaveFileResult = \_ _ -> Right (),
      msSavedFiles = Map.empty,
      msStdinQueue = [],
      msSessionPath = Right "/tmp/session.txt",
      msTask = Right (Task (ContestId "abc100") (ProblemId "a")),
      msLoadSessionResult = Left SessionNotFound,
      msSaveSessionResult = \_ -> Right (),
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

  createDirectoryIfMissing missing path = do
    resultFunc <- gets msCreateDirIfMissing
    case resultFunc missing path of
      Right () -> do
        modify $ \s -> s {msCreatedDirs = Set.insert path (msCreatedDirs s)}
        pure $ Right ()
      Left err -> pure $ Left err

  doesFileExist _path = do
    queue <- gets msDoesFileExistQueue
    case queue of
      [] -> error "MockApp: doesFileExist called on empty queue"
      (x : xs) -> do
        modify $ \s -> s {msDoesFileExistQueue = xs}
        pure x
  getCurrentDirectory = gets msCurrentDir
  readFile _filePath = gets msReadFiles
  saveFile path content = do
    resultFunc <- gets msSaveFileResult
    case resultFunc path content of
      Right () -> do
        modify $ \s -> s {msSavedFiles = Map.insert path content (msSavedFiles s)}
        pure $ Right ()
      Left err -> pure $ Left err

instance HasConfig MockApp where
  loadSessionPath = gets msSessionPath
  loadTask = gets msTask

instance HasSession MockApp where
  loadSession = gets msLoadSessionResult
  saveSession session = do
    resultFunc <- gets msSaveSessionResult
    case resultFunc session of
      Right () -> do
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

instance HasUser MockApp where
  sendMsg msg = modify $ \s -> s {msMsgs = msMsgs s ++ [msg]}