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
    submitPageUrlIO,
  )
where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Functor ((<&>))
import Data.List (isInfixOf)
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.Encoding as TEnc
import qualified Data.Text.IO as TIO
import Interface (MonadReq (..))
import Network.HTTP.Req (HttpConfig (..), defaultHttpConfig, ignoreResponse, responseStatusCode)
import Text.HTML.TagSoup
import Text.Regex.TDFA
import Text.URI
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
    >>= either (pure . Left) parseTestCases

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

submitPageUrlIO :: (MonadThrow m) => Task -> m URI
submitPageUrlIO (Task (ContestId cid) (ProblemId pid)) =
  mkURI $
    "https://atcoder.jp/contests/"
      <> cid
      <> "/submit?taskScreenName="
      <> T.replace "-" "_" cid
      <> "_"
      <> pid

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

parseTestCases :: (MonadIO m) => T.Text -> m (Either AppError [TestCase])
parseTestCases body = do
  liftIO $ TIO.putStrLn "Parsing HTML response for test cases..."
  return $ go body
  where
    go :: T.Text -> Either AppError [TestCase]
    go html =
      pairUp (parseSamples html)
        <&> map mkTestCase . zipPairs

    mkTestCase :: (Int, (T.Text, T.Text)) -> TestCase
    mkTestCase (i, (input, output)) =
      TestCase
        { tcName = T.pack (show i),
          tcInput = TEnc.encodeUtf8 input,
          tcOutput = TEnc.encodeUtf8 output
        }

    pairUp :: [a] -> Either AppError [(a, a)]
    pairUp [] = Right []
    pairUp (x : y : rest) = ((x, y) :) <$> pairUp rest
    pairUp _ = Left $ ProviderError "pairUp: odd length list"

    zipPairs :: [(a, a)] -> [(Int, (a, a))]
    zipPairs = zip [1 ..]

    parseSamples :: T.Text -> [T.Text]
    parseSamples html =
      go (parseTags (T.unpack html)) []
      where
        go [] acc = acc
        go (TagOpen "h3" _ : TagText heading : TagClose "h3" : rest) acc
          | "入力例" `isInfixOf` heading || "出力例" `isInfixOf` heading =
              case extractPre rest of
                (Just content, rest') -> go rest' (acc ++ [content])
                (Nothing, rest') -> go rest' acc
        go (_ : xs) acc = go xs acc

        extractPre (TagOpen "pre" _ : TagText content : TagClose "pre" : rest) =
          (Just $ T.pack content, rest)
        extractPre (_x : xs) = extractPre xs
        extractPre _ = (Nothing, [])