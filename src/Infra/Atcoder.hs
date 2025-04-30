-- src/Infrastructure/HttpAtCoder.hs
{-# LANGUAGE OverloadedStrings #-}

module Infra.Atcoder
  ( fetchProblemIdsIO,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text.IO as TIO
import Domain

-- | Dummy IO implementation for fetchProblemList
fetchProblemIdsIO :: (MonadIO m) => ContestId -> m (Either DomainError [ProblemId])
fetchProblemIdsIO c = liftIO $ do
  TIO.putStrLn $ ">>> Simulating: Fetching problems for " <> deContestId c
  -- ダミー実装: 固定のリストを返す (成功例)
  pure $ sequenceA [toProblemId "a", toProblemId "b"]