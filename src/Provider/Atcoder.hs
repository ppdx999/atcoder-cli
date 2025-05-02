{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Provider.Atcoder
  ( fetchProblemIdsDummyIO,
    fetchTestCasesDummyIO,
    fetchProblemIdsIO,
    fetchTestCasesIO,
    AtCoderEnv (..),
    createAtCoderEnv,
  )
where

import Control.Exception (IOException)
import Control.Monad.Catch (MonadThrow, SomeException, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Except (runExceptT, throwE)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.List as List
-- For decoding ByteString

import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TEnc
import qualified Data.Text.IO as TIO
import Interface (MonadReq (..))
import Network.HTTP.Req
  ( GET (GET),
    NoReqBody (NoReqBody),
    Url (..),
    bsResponse,
    defaultHttpConfig,
    req,
    responseBody,
    runReq,
    useHttpsURI,
    useURI,
  )
import Text.Regex.TDFA
import Text.Regex.TDFA.Text
import Text.URI (URI, mkURI)
import Types

data AtCoderEnv = AtCoderEnv
  {
  }
  deriving (Show)

-- | AtCoderEnv を初期化する IO アクション (現状は空)
createAtCoderEnv :: (MonadIO m) => m AtCoderEnv
createAtCoderEnv = pure AtCoderEnv {} -- 今は特に設定するものがない

-- | Dummy IO implementation for fetchProblemList
fetchProblemIdsDummyIO :: (MonadIO m) => ContestId -> m (Either AppError [ProblemId])
fetchProblemIdsDummyIO c = liftIO $ do
  TIO.putStrLn $ ">>> Simulating: Fetching problems for " <> deContestId c
  pure $ sequenceA [toProblemId "a", toProblemId "b"]

-- | Dummy IO implementation for fetchTestCases
fetchTestCasesDummyIO :: (MonadIO m) => Task -> m (Either AppError [TestCase])
fetchTestCasesDummyIO task = liftIO $ do
  TIO.putStrLn $ ">>> Simulating: Fetching test cases for " <> deContestId (taskContestId task) <> "/" <> deProblemId (taskProblemId task)
  let tc1 = TestCase "sample1" (BSC.pack "1 2\n") (BSC.pack "3\n")
  let tc2 = TestCase "sample2" (BSC.pack "10 20\n") (BSC.pack "30\n")
  pure $ Right [tc1, tc2]

-- | Real IO implementation for fetchProblemList (骨格 - req + Regex 想定)
fetchProblemIdsIO ::
  ( MonadIO m,
    MonadReq m
  ) =>
  AtCoderEnv ->
  ContestId ->
  m (Either AppError [ProblemId])
fetchProblemIdsIO env contestId =
  fetchTaskPage env contestId
    >>= either (pure . Left) parseProblemIdsWithRegex

-- | Real IO implementation for fetchTestCases (骨格 - req + Regex 想定)
fetchTestCasesIO ::
  (MonadIO m, MonadReq m) =>
  AtCoderEnv ->
  Task ->
  m (Either AppError [TestCase])
fetchTestCasesIO _env task = do
  let contest = deContestId (taskContestId task)
  let problem = deProblemId (taskProblemId task)
  let url = "https://atcoder.jp/contests/" <> T.unpack contest <> "/tasks/" <> T.unpack contest <> "_" <> T.unpack problem
  liftIO $ TIO.putStrLn $ "[Skeleton] Fetching test cases from: " <> T.pack url

  eResponse <- reqGet url

  case eResponse of
    Left err -> pure $ Left err
    Right body -> do
      liftIO $ TIO.putStrLn "[Skeleton] Parsing HTML response for test cases using Regex..."
      -- ここで正規表現を使って TestCase を抽出する
      let eTestCases = parseTestCasesWithRegex body -- 仮の呼び出し
      pure eTestCases

-- --- Page Fetching Helpers ---
fetchTaskPage :: (MonadIO m, MonadReq m) => AtCoderEnv -> ContestId -> m (Either AppError BS.ByteString)
fetchTaskPage _env contestId = do
  let url = "https://atcoder.jp/contests/" <> T.unpack (deContestId contestId) <> "/tasks"
  liftIO $ TIO.putStrLn $ "Fetching problems from: " <> T.pack url
  reqGet url

-- --- Regex Parsing Helpers ---

-- | Parses Problem IDs from HTML ByteString using regex.
parseProblemIdsWithRegex :: (MonadIO m) => BSC.ByteString -> m (Either AppError [ProblemId])
parseProblemIdsWithRegex body = do
  liftIO $ TIO.putStrLn "[Skeleton] Parsing HTML response for test cases using Regex..."
  return $ parseProblemIdsWithRegex' body

parseProblemIdsWithRegex' :: BSC.ByteString -> Either AppError [ProblemId]
parseProblemIdsWithRegex' body =
  traverse toProblemId
    . List.nub
    . map (T.takeWhileEnd (/= '_'))
    $ getAllTextMatches (decoded =~ pattern)
  where
    decoded = TEnc.decodeUtf8 body
    pattern :: T.Text
    pattern = "/contests/[^/]+/tasks/[^/]+_[a-zA-Z0-9]+"

parseTestCasesWithRegex :: BSC.ByteString -> Either AppError [TestCase]
parseTestCasesWithRegex _body =
  -- ここに正規表現を使ったパース処理を実装
  Left (ProviderError "Test case parsing (Regex) not implemented yet.")
