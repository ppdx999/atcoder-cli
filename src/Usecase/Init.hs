{-# LANGUAGE OverloadedStrings #-}

module Usecase.Init
  ( initContest,
  )
where

import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import Data.Foldable (traverse_)
import qualified Data.Text as T
import Interface
import System.FilePath ((</>))
import Types

initContest ::
  ( HasAtcoder m,
    HasFileSystem m,
    HasLogger m
  ) =>
  ContestId ->
  m (Either AppError ())
initContest contestId@(ContestId contestIdText) = runExceptT $ do
  logInfoE $ "Initializing contest: " <> contestIdText

  logInfoE $ "Creating directory: " <> contestIdText
  ExceptT $ createDirectory' contestIdText

  logInfoE "Fetching problem list..."
  problemIds <- ExceptT $ fetchProblemIds contestId
  logInfoE $ "Found " <> T.pack (show $ length problemIds) <> " problems."

  traverse_ createProblemDir problemIds
  where
    logInfoE :: (HasLogger m, MonadTrans t) => T.Text -> t m ()
    logInfoE = lift . logInfo
    createDirectory' :: (HasFileSystem m) => T.Text -> m (Either AppError ())
    createDirectory' = createDirectory . T.unpack
    createProblemDir :: (HasFileSystem m, HasLogger m) => ProblemId -> ExceptT AppError m ()
    createProblemDir (ProblemId problemIdText) = do
      let dir = filepathJoin contestIdText problemIdText
      logInfoE $ "Creating directory: " <> dir
      ExceptT $ createDirectory' dir
    filepathJoin :: T.Text -> T.Text -> T.Text
    filepathJoin parent child = T.pack (T.unpack parent </> T.unpack child)