{-# LANGUAGE MultiParamTypeClasses #-}

module Usecase.Ports
  ( HasAtcoder (..),
    HasFileSystem (..),
    HasLogger (..),
  )
where

import Control.Monad.Catch (MonadThrow)
import Data.Text (Text)
import Domain

class (Monad m) => HasLogger m where
  logInfo :: Text -> m ()
  logError :: Text -> m ()

class (Monad m, MonadThrow m) => HasFileSystem m where
  createDirectory :: FilePath -> m (Either DomainError ())


class (Monad m) => HasAtcoder m where
  fetchProblemIds :: ContestId -> m (Either DomainError [ProblemId])

