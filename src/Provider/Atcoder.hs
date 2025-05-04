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

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.Encoding as TEnc
import qualified Data.Text.IO as TIO
import Interface (MonadReq (..))
import Text.Regex.TDFA
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
fetchProblemIdsDummyIO (ContestId contestIdText) = liftIO $ do
  TIO.putStrLn $ ">>> Simulating: Fetching problems for " <> contestIdText
  pure $ sequenceA [validateProblemId "a", validateProblemId "b"]

-- | Dummy IO implementation for fetchTestCases
fetchTestCasesDummyIO :: (MonadIO m) => Task -> m (Either AppError [TestCase])
fetchTestCasesDummyIO (Task (ContestId c) (ProblemId p)) = liftIO $ do
  TIO.putStrLn $ ">>> Simulating: Fetching test cases for " <> c <> "/" <> p
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
fetchTestCasesIO env task = do
  fetchProblemPage env task
    >>= either (pure . Left) parseTestCasesWithRegex

-- --- Page Fetching Helpers ---
fetchTaskPage :: (MonadIO m, MonadReq m) => AtCoderEnv -> ContestId -> m (Either AppError BS.ByteString)
fetchTaskPage _env (ContestId cid) = do
  let url = "https://atcoder.jp/contests/" <> T.unpack cid <> "/tasks"
  liftIO $ TIO.putStrLn $ "Fetching problems from: " <> T.pack url
  reqGet url

fetchProblemPage :: (MonadIO m, MonadReq m) => AtCoderEnv -> Task -> m (Either AppError BS.ByteString)
fetchProblemPage _env (Task (ContestId cid) (ProblemId pid)) = do
  let url =
        "https://atcoder.jp/contests/"
          <> T.unpack cid
          <> "/tasks/"
          <> T.unpack (T.replace "-" "_" cid)
          <> "_"
          <> T.unpack pid
  liftIO $ TIO.putStrLn $ "[Skeleton] Fetching test cases from: " <> T.pack url
  reqGet url

-- --- Regex Parsing Helpers ---

-- | Parses Problem IDs from HTML ByteString using regex.
parseProblemIdsWithRegex :: (MonadIO m) => BSC.ByteString -> m (Either AppError [ProblemId])
parseProblemIdsWithRegex body = do
  liftIO $ TIO.putStrLn "[Skeleton] Parsing HTML response for test cases using Regex..."
  return $ parseProblemIdsWithRegex' body

parseProblemIdsWithRegex' :: BSC.ByteString -> Either AppError [ProblemId]
parseProblemIdsWithRegex' body =
  traverse validateProblemId
    . List.nub
    . map (T.takeWhileEnd (/= '_'))
    $ getAllTextMatches (decoded =~ pattern)
  where
    decoded = TEnc.decodeUtf8 body
    pattern :: T.Text
    pattern = "/contests/[^/]+/tasks/[^/]+_[a-zA-Z0-9]+"

parseTestCasesWithRegex :: (MonadIO m) => BSC.ByteString -> m (Either AppError [TestCase])
parseTestCasesWithRegex body = do
  liftIO $ TIO.putStrLn "Parsing HTML response for test cases using Regex..."
  return $ parseTestCasesWithRegex' body

parseTestCasesWithRegex' :: BSC.ByteString -> Either AppError [TestCase]
parseTestCasesWithRegex' body = do
  pairs <-
    pairUp
      . map extractPreContents
      . extractSampleLine
      . TEnc.decodeUtf8
      $ body
  Right (map mkTestCase $ zipPairs pairs)
  where
    extractSampleLine :: T.Text -> [T.Text]
    extractSampleLine html = getAllTextMatches (html =~ pattern) :: [T.Text]
      where
        pattern :: T.Text
        pattern = "<h3>(入力例|出力例)[[:space:]]*[[:digit:]]+</h3>[[:space:]]*<pre>([[:print:][:space:]]*)[[:space:]]*</pre>"

    extractPreContents :: T.Text -> T.Text
    extractPreContents line = case matches of
      (_, _, _, [content]) -> T.strip content
      _ -> ""
      where
        pattern' :: T.Text
        pattern' = "<pre>([[:print:][:space:]]*)[[:space:]]*</pre>"
        matches = line =~ pattern' :: (T.Text, T.Text, T.Text, [T.Text])

    mkTestCase :: (Int, (T.Text, T.Text)) -> TestCase
    mkTestCase (i, (input, output)) =
      TestCase
        { tcName = T.pack (show i),
          tcInput = TEnc.encodeUtf8 $ wrapNewLine input,
          tcOutput = TEnc.encodeUtf8 $ wrapNewLine output
        }

    wrapNewLine :: T.Text -> T.Text
    wrapNewLine txt
      | T.isSuffixOf "\n" txt = txt
      | otherwise = txt <> "\n"

    pairUp :: [a] -> Either AppError [(a, a)]
    pairUp [] = Right []
    pairUp (x : y : rest) = ((x, y) :) <$> pairUp rest
    pairUp _ = Left $ ProviderError "pairUp: odd length list"

    zipPairs :: [(a, a)] -> [(Int, (a, a))]
    zipPairs = zip [1 ..]