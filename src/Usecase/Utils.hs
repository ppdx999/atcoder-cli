-- src/Usecase/Utils.hs
{-# LANGUAGE OverloadedStrings #-}

module Usecase.Utils
  ( parseTaskFromPath,
  )
where

import qualified Data.Text as T
import System.FilePath (pathSeparator, takeBaseName, takeDirectory)
import Types -- Types.hs をインポート

-- | Parses ContestId and ProblemId from the last two directory components of a FilePath.
-- Example: "/path/to/abc100/a/" -> Right Task { taskContestId = "abc100", taskProblemId = "a" }
parseTaskFromPath :: FilePath -> Either AppError Task
parseTaskFromPath fp = do
  let normalizedPath = dropTrailingSeparator fp
  let problemIdStr = takeBaseName normalizedPath
  let contestIdStr = takeBaseName (takeDirectory normalizedPath)

  if null contestIdStr || null problemIdStr
    then Left (ProviderError ("Could not parse contest/problem ID from path: " <> T.pack fp))
    else do
      contestId <- toContestId (T.pack contestIdStr)
      problemId <- toProblemId (T.pack problemIdStr)
      pure Task {taskContestId = contestId, taskProblemId = problemId}

-- | Helper to remove trailing path separator if present.
dropTrailingSeparator :: FilePath -> FilePath
dropTrailingSeparator p =
  if isTrailingSeparator p && length p > 1
    then take (length p - 1) p
    else p
  where
    isTrailingSeparator path = case reverse path of
      (c : _) -> c == pathSeparator
      [] -> False
