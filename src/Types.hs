{-# LANGUAGE OverloadedStrings #-}

module Types
  ( ContestId (..),
    validateContestId,
    ProblemId (..),
    validateProblemId,
    Task (..),
    Language (..),
    Cmd (..),
    Stdin (..),
    Stdout (..),
    RunTestCaseResult (..),
    Session (..),
    validateSession,
    TestCase (..),
    SubmissionId (..),
    SubmissionState (..),
    Submission (..),
    AppError (..),
  )
where

import Data.ByteString (ByteString)
import qualified Data.Text as T
import Data.Time (UTCTime)

-- | Contest ID (e.g., "abc100")
newtype ContestId = ContestId T.Text
  deriving (Eq, Ord, Show)

-- | Problem ID (e.g., "a", "b", "c")
newtype ProblemId = ProblemId T.Text
  deriving (Eq, Ord, Show)

-- | Represents a specific problem within a contest
data Task = Task
  { taskContestId :: ContestId,
    taskProblemId :: ProblemId
  }
  deriving (Eq, Ord, Show)

newtype RunTestCaseResult = RunTestCaseResult ByteString
  deriving (Eq, Ord, Show)

-- | Session Cookie as raw text
newtype Session = Session T.Text
  deriving (Eq, Ord, Show)

-- | A single test case (input/output pair)
data TestCase = TestCase
  { tcName :: T.Text, -- e.g., "sample1"
    tcInput :: ByteString,
    tcOutput :: ByteString
  }
  deriving (Eq, Show)

data Language = Language
  { langName :: T.Text,
    sourceFile :: String,
    buildCmd :: Maybe Cmd,
    builtFile :: Maybe FilePath,
    runCmd :: Cmd
  }
  deriving (Eq, Ord, Show)

newtype Cmd = Cmd [String]
  deriving (Eq, Ord, Show)

newtype Stdin = Stdin ByteString
  deriving (Eq, Ord, Show)

newtype Stdout = Stdout ByteString
  deriving (Eq, Ord, Show)

-- | Submission ID
newtype SubmissionId = SubmissionId Int
  deriving (Eq, Ord, Show)

-- | State of a submission (Accepted, Wrong Answer, etc.)
data SubmissionState
  = Waiting
  | Judging
  | AC
  | WA
  | TLE
  | MLE
  | RE
  | CE
  deriving (Eq, Ord, Show)

-- | A submission record
data Submission = Submission
  { submId :: SubmissionId,
    submState :: SubmissionState,
    submTimestamp :: UTCTime
  }
  deriving (Eq, Show)

-- | Types-layer errors
data AppError
  = InvalidContestId T.Text
  | InvalidProblemId T.Text
  | InvalidSession T.Text
  | SessionNotFound
  | ProviderError T.Text
  deriving (Eq, Show)

validateContestId :: T.Text -> Either AppError ContestId
validateContestId t
  | T.null t = Left (InvalidContestId "Contest ID cannot be empty.")
  | T.all validChar t = Right (ContestId t)
  | otherwise = Left (InvalidContestId ("Invalid characters in Contest ID: " <> t))
  where
    validChars :: T.Text
    validChars = "abcdefghijklmnopqrstuvwxyz0123456789-"
    validChar :: Char -> Bool
    validChar c = T.any (== c) validChars

validateProblemId :: T.Text -> Either AppError ProblemId
validateProblemId t
  | T.null t = Left (InvalidProblemId "Problem ID cannot be empty.")
  | T.all validChar t = Right (ProblemId t)
  | otherwise = Left (InvalidProblemId ("Invalid characters in Problem ID: " <> t))
  where
    validChars :: T.Text
    validChars = "abcdefghijklmnopqrstuvwxyz"
    validChar :: Char -> Bool
    validChar c = T.any (== c) validChars

validateSession :: T.Text -> Either AppError Session
validateSession t
  | T.null t = Left (InvalidSession "Session cannot be empty.")
  | otherwise = Right (Session t)