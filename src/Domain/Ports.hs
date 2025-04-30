module Domain.Ports
  ( HasFetchProblemList (..),
    HasCreateDirectory (..),
    HasLogInfo (..),
  )
where

import Control.Monad.Catch (MonadThrow)
import Data.Text
import Domain.Types

class (Monad m) => HasFetchProblemList m where
  fetchProblemList :: ContestId -> m (Either DomainError [ProblemId])

class (Monad m, MonadThrow m) => HasCreateDirectory m where
  createDirectory :: FilePath -> m (Either DomainError ())

class (Monad m) => HasLogInfo m where
  logInfo :: Text -> m ()