-- src/Providerstructure/HttpAtCoder.hs
{-# LANGUAGE OverloadedStrings #-}

module Provider.Atcoder
  ( fetchProblemIdsIO,
    fetchTestCasesIO,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text.IO as TIO
import Types

-- | Dummy IO implementation for fetchProblemList
fetchProblemIdsIO :: (MonadIO m) => ContestId -> m (Either AppError [ProblemId])
fetchProblemIdsIO c = liftIO $ do
  TIO.putStrLn $ ">>> Simulating: Fetching problems for " <> deContestId c
  -- ダミー実装: 固定のリストを返す (成功例)
  pure $ sequenceA [toProblemId "a", toProblemId "b"]

fetchTestCasesIO :: (MonadIO m) => Task -> m (Either AppError [TestCase])
fetchTestCasesIO task = liftIO $ do
  TIO.putStrLn $ ">>> Simulating: Fetching test cases for " <> deContestId (taskContestId task) <> "/" <> deProblemId (taskProblemId task)
  -- ダミー実装: 固定のテストケースリストを返す
  let tc1 = TestCase "sample1" (BSC.pack "1 2\n") (BSC.pack "3\n")
  let tc2 = TestCase "sample2" (BSC.pack "10 20\n") (BSC.pack "30\n")
  pure $ Right [tc1, tc2]