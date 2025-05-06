{-# LANGUAGE MultiParamTypeClasses #-}

module Interface
  ( HasAtcoder (..),
    HasFileSystem (..),
    HasLogger (..),
    MonadReq (..),
    HasStdin (..),
    HasSession (..),
  )
where

import Control.Monad.Catch (MonadThrow)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import Data.Text (Text)
import Types

class (Monad m) => HasLogger m where
  logInfo :: Text -> m ()
  logError :: Text -> m ()

class (Monad m, MonadThrow m) => HasFileSystem m where
  createDirectory :: FilePath -> m (Either AppError ())
  getCurrentDirectory :: m FilePath
  readFile :: FilePath -> m (Either AppError ByteString)
  saveFile :: FilePath -> ByteString -> m (Either AppError ())

class (Monad m) => HasSession m where
  loadSession :: m (Either AppError Session)
  saveSession :: Session -> m (Either AppError ())

class (Monad m) => HasAtcoder m where
  fetchProblemIds :: ContestId -> m (Either AppError [ProblemId])
  fetchTestCases :: Task -> m (Either AppError [TestCase])
  verifySession :: Session -> m (Either AppError Bool)

class (Monad m) => MonadReq m where
  reqGet :: String -> m (Either AppError BSC.ByteString)

class (Monad m) => HasStdin m where
  readLine :: m Text