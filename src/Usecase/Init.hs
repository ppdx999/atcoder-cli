module Usecase.Init
  ( initContest,
  )
where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import Data.Foldable (traverse_)
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
initContest contestId@(ContestId cid) = runExceptT $ do
  let createProblemDir (ProblemId pid) =
        ExceptT $ createDirectory (cid </> pid)

  lift $ logInfo $ "Initializing contest: " <> cid
  ExceptT $ createDirectory cid

  ExceptT (fetchProblemIds contestId)
    >>= traverse_ createProblemDir