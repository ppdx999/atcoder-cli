{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Provider.Atcoder
  ( fetchProblemIdsIO,
    fetchTestCasesIO,
    verifySessionIO,
    AtCoderEnv (..),
    createAtCoderEnv,
  )
where

import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.Encoding as TEnc
import qualified Data.Text.IO as TIO
import Interface (MonadReq (..))
import Network.HTTP.Req (HttpConfig (..), defaultHttpConfig, ignoreResponse, responseStatusCode)
import Text.Regex.TDFA
import Types

data AtCoderEnv = AtCoderEnv
  {
  }
  deriving (Show)

-- | AtCoderEnv を初期化する IO アクション (現状は空)
createAtCoderEnv :: (MonadIO m) => m AtCoderEnv
createAtCoderEnv = pure AtCoderEnv {} -- 今は特に設定するものがない

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

verifySessionIO ::
  (MonadIO m, MonadReq m) =>
  AtCoderEnv ->
  Session ->
  m (Either AppError Bool)
verifySessionIO _env session = runExceptT $ do
  liftIO $ TIO.putStrLn "Verifying session with AtCoder..."
  let url = "https://atcoder.jp/settings"
  let config =
        defaultHttpConfig
          { httpConfigRedirectCount = 0
          }

  res <- ExceptT $ reqGetWithSession session url ignoreResponse config mempty

  let statusCode = responseStatusCode res
  liftIO $ TIO.putStrLn $ "Response Status Code: " <> T.pack (show statusCode)
  pure $ statusCode == 200

-- --- Page Fetching Helpers ---
fetchTaskPage :: (MonadIO m, MonadReq m) => AtCoderEnv -> ContestId -> m (Either AppError T.Text)
fetchTaskPage _env (ContestId cid) = do
  let url = "https://atcoder.jp/contests/" <> T.unpack cid <> "/tasks"
  liftIO $ TIO.putStrLn $ "Fetching problems from: " <> T.pack url
  getHtml url

fetchProblemPage :: (MonadIO m, MonadReq m) => AtCoderEnv -> Task -> m (Either AppError T.Text)
fetchProblemPage _env (Task (ContestId cid) (ProblemId pid)) = do
  let url =
        "https://atcoder.jp/contests/"
          <> T.unpack cid
          <> "/tasks/"
          <> T.unpack (T.replace "-" "_" cid)
          <> "_"
          <> T.unpack pid
  liftIO $ TIO.putStrLn $ "[Skeleton] Fetching test cases from: " <> T.pack url
  getHtml url

-- --- Regex Parsing Helpers ---

-- | Parses Problem IDs from HTML ByteString using regex.
parseProblemIdsWithRegex :: (MonadIO m) => T.Text -> m (Either AppError [ProblemId])
parseProblemIdsWithRegex body = do
  liftIO $ TIO.putStrLn "[Skeleton] Parsing HTML response for test cases using Regex..."
  return $ parseProblemIdsWithRegex' body

parseProblemIdsWithRegex' :: T.Text -> Either AppError [ProblemId]
parseProblemIdsWithRegex' html =
  traverse validateProblemId
    . List.nub
    . map (T.takeWhileEnd (/= '_'))
    $ getAllTextMatches (html =~ pattern)
  where
    pattern :: T.Text
    pattern = "/contests/[^/]+/tasks/[^/]+_[a-zA-Z0-9]+"

parseTestCasesWithRegex :: (MonadIO m) => T.Text -> m (Either AppError [TestCase])
parseTestCasesWithRegex body = do
  liftIO $ TIO.putStrLn "Parsing HTML response for test cases using Regex..."
  return $ parseTestCasesWithRegex' body

parseTestCasesWithRegex' :: T.Text -> Either AppError [TestCase]
parseTestCasesWithRegex' html = do
  pairs <-
    pairUp
      . map extractPreContents
      . extractSampleLine
      $ html
  Right (map mkTestCase $ zipPairs pairs)
  where
    extractSampleLine :: T.Text -> [T.Text]
    extractSampleLine html' = getAllTextMatches (html' =~ pattern) :: [T.Text]
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