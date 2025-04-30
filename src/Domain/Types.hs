module Domain.Types
  ( ContestId (..),
    ProblemId (..),
    Task (..),
    LanguageId (..),
    SourceFile (..),
    SessionCookie (..),
    TestCase (..),
    SubmissionId (..),
    SubmissionState (..),
    Submission (..),
    DomainError (..),
  )
where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time (UTCTime)

-- | Contest ID (e.g., "abc100")
newtype ContestId = ContestId Text
  deriving (Eq, Ord, Show)

-- | Problem ID (e.g., "a", "b", "c")
newtype ProblemId = ProblemId Text
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
newtype SessionCookie = SessionCookie Text
  deriving (Eq, Ord, Show)

-- | A single test case (input/output pair)
data TestCase = TestCase
  { tcName :: Text, -- e.g., "sample1"
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

-- | Domain-layer errors
data DomainError
  = InvalidContestId Text
  | InvalidProblemId Text
  | InvalidLanguageId Int
  | SourceFileNotFound FilePath
  | CookieParseFailed Text
  | DuplicateTestCase Text
  | UnexpectedState Text
  | InfraError Text
  deriving (Eq, Show)
