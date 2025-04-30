-- src/Infrastructure/HttpAtCoder.hs
{-# LANGUAGE OverloadedStrings #-}

module Infra.Atcoder
  ( fetchProblemIdsIO,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text.IO as TIO
import Domain.Types

-- | Dummy IO implementation for fetchProblemList
fetchProblemIdsIO :: (MonadIO m) => ContestId -> m (Either DomainError [ProblemId])
fetchProblemIdsIO contestId@(ContestId cid) = liftIO $ do
  TIO.putStrLn $ ">>> Simulating: Fetching problems for " <> cid
  -- ここで実際には HTTP GET リクエストを送信し、
  -- レスポンスボディ (HTML) をパースして ProblemId のリストを抽出する
  -- 例: wreq や http-client + tagsoup/scalpel などを使用

  -- ダミー実装: 固定のリストを返す (成功例)
  pure $ Right [ProblemId "a", ProblemId "b"]

-- ダミー実装: エラーを返す (失敗例)
-- pure $ Left (InfraError "Failed to connect to AtCoder (dummy)")
