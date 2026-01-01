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
    OS (..),
  )
where

import Data.Time (UTCTime)

-- | Contest ID (e.g., "abc100")
newtype ContestId = ContestId String
  deriving (Eq, Ord, Show)

-- | Problem ID (e.g., "a", "b", "c")
newtype ProblemId = ProblemId String
  deriving (Eq, Ord, Show)

-- | Represents a specific problem within a contest
data Task = Task
  { taskContestId :: ContestId,
    taskProblemId :: ProblemId
  }
  deriving (Eq, Ord, Show)

newtype RunTestCaseResult = RunTestCaseResult String
  deriving (Eq, Ord, Show)

newtype Session = Session String
  deriving (Eq, Ord, Show)

data TestCase = TestCase
  { tcName :: String, -- e.g., "sample1"
    tcInput :: String,
    tcOutput :: String
  }
  deriving (Eq, Show)

data Language = Language
  { langName :: String,
    sourceFile :: String,
    buildCmd :: Maybe Cmd,
    builtFile :: Maybe FilePath,
    runCmd :: Cmd
  }
  deriving (Eq, Ord, Show)

newtype Cmd = Cmd [String]
  deriving (Eq, Ord, Show)

newtype Stdin = Stdin String
  deriving (Eq, Ord, Show)

newtype Stdout = Stdout String
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
  = InvalidContestId String
  | InvalidProblemId String
  | InvalidSession String
  | SessionNotFound
	| InvalidArgument String
  | ProviderError String
  deriving (Eq, Show)

data OS = Linux | WSL | Mac | Windows deriving (Show, Eq)

validateContestId :: String -> Either AppError ContestId
validateContestId t
  | null t = Left (InvalidContestId "Contest ID cannot be empty.")
  | all validChar t = Right (ContestId t)
  | otherwise = Left (InvalidContestId ("Invalid characters in Contest ID: " <> t))
  where
    validChars :: String
    validChars = "abcdefghijklmnopqrstuvwxyz0123456789-"
    validChar :: Char -> Bool
    validChar c = c `elem` validChars

validateProblemId :: String -> Either AppError ProblemId
validateProblemId t
  | null t = Left (InvalidProblemId "Problem ID cannot be empty.")
  | all validChar t = Right (ProblemId t)
  | otherwise = Left (InvalidProblemId ("Invalid characters in Problem ID: " <> t))
  where
    validChars :: String
    validChars = "abcdefghijklmnopqrstuvwxyz0123456789_"
    validChar :: Char -> Bool
    validChar c = c `elem` validChars

validateSession :: String -> Either AppError Session
validateSession t
  | null t = Left (InvalidSession "Session cannot be empty.")
  | otherwise = Right (Session t)
