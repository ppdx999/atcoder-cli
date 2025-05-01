{-# LANGUAGE OverloadedStrings #-}

module Types
  ( ContestId,
    toContestId,
    deContestId,
    ProblemId,
    toProblemId,
    deProblemId,
    Task (..),
    LanguageId,
    toLanguageId,
    SourceFile,
    toSourceFile,
    SessionCookie (..),
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

-- | Language ID (e.g., 4001 for C++, 4011 for Python)
newtype LanguageId = LanguageId Int
  deriving (Eq, Ord, Show)

-- | Source file path (e.g., "./main.cpp")
newtype SourceFile = SourceFile FilePath
  deriving (Eq, Ord, Show)

-- | Session Cookie as raw text
newtype SessionCookie = SessionCookie T.Text
  deriving (Eq, Ord, Show)

-- | A single test case (input/output pair)
data TestCase = TestCase
  { tcName :: T.Text, -- e.g., "sample1"
    tcInput :: ByteString,
    tcOutput :: ByteString
  }
  deriving (Eq, Show)

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
  | InvalidLanguageId Int
  | SourceFileNotFound FilePath
  | CookieParseFailed T.Text
  | DuplicateTestCase T.Text
  | UnexpectedState T.Text
  | ProviderError T.Text
  deriving (Eq, Show)

toContestId :: T.Text -> Either AppError ContestId
toContestId t
  | T.null t = Left (InvalidContestId "Contest ID cannot be empty.")
  | T.all validChar t = Right (ContestId t)
  | otherwise = Left (InvalidContestId ("Invalid characters in Contest ID: " <> t))
  where
    validChars :: T.Text
    validChars = "abcdefghijklmnopqrstuvwxyz0123456789-"
    validChar :: Char -> Bool
    validChar c = T.any (== c) validChars

deContestId :: ContestId -> T.Text
deContestId (ContestId t) = t

toProblemId :: T.Text -> Either AppError ProblemId
toProblemId t
  | T.null t = Left (InvalidProblemId "Problem ID cannot be empty.")
  | T.all validChar t = Right (ProblemId t)
  | otherwise = Left (InvalidProblemId ("Invalid characters in Problem ID: " <> t))
  where
    validChars :: T.Text
    validChars = "abcdefghijklmnopqrstuvwxyz"
    validChar :: Char -> Bool
    validChar c = T.any (== c) validChars

deProblemId :: ProblemId -> T.Text
deProblemId (ProblemId p) = p

toLanguageId :: Int -> Either AppError LanguageId
toLanguageId n
  | n > 0 = Right (LanguageId n)
  | otherwise = Left (InvalidLanguageId n)

toSourceFile :: FilePath -> Either AppError SourceFile
toSourceFile path
  | null path = Left (SourceFileNotFound path)
  | otherwise = Right (SourceFile path)