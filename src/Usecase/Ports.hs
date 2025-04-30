module Usecase.Ports
  ( HasFetchProblemIds (..),
    HasCreateDirectory (..),
    HasLogger (..),
  )
where

import Control.Monad.Catch (MonadThrow)
import Data.Text
import Domain.Types

class (Monad m) => HasFetchProblemIds m where
  fetchProblemIds :: ContestId -> m (Either DomainError [ProblemId])

class (Monad m, MonadThrow m) => HasCreateDirectory m where
  createDirectory :: FilePath -> m (Either DomainError ())

class (Monad m) => HasLogger m where
  logInfo :: Text -> m ()
  logError :: Text -> m ()