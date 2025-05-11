{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (MonadTrans (lift))
import Data.Functor ((<&>))
import Data.List (isInfixOf, isPrefixOf)
import qualified Data.List as List
import Interface
import Network.HTTP.Req (HttpConfig (..), defaultHttpConfig, ignoreResponse, responseStatusCode)
import Provider.Utils (dashToUnderscore, mkURI, takeWhileEnd)
import Text.HTML.TagSoup
import Text.URI (URI)
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
  (MonadIO m, MonadReq m, HasLogger m) =>
  AtCoderEnv ->
  ContestId ->
  m (Either AppError [ProblemId])
fetchProblemIdsIO env contestId =
  fetchTaskPage env contestId
    >>= either (pure . Left) parseProblemIds

-- | Real IO implementation for fetchTestCases (骨格 - req + Regex 想定)
fetchTestCasesIO ::
  (MonadIO m, MonadReq m, HasLogger m) =>
  AtCoderEnv ->
  Task ->
  m (Either AppError [TestCase])
fetchTestCasesIO env task = do
  fetchProblemPage env task
    >>= either (pure . Left) parseTestCases

verifySessionIO ::
  (MonadIO m, MonadReq m, HasLogger m) =>
  AtCoderEnv ->
  Session ->
  m (Either AppError Bool)
verifySessionIO _env session = runExceptT $ do
  lift $ logInfo "Verifying session with AtCoder..."
  let url = "https://atcoder.jp/settings"
  let config =
        defaultHttpConfig
          { httpConfigRedirectCount = 0
          }

  res <- ExceptT $ reqGetWithSession session url ignoreResponse config mempty

  let statusCode = responseStatusCode res
  lift $ logInfo $ "Response Status Code: " <> show statusCode
  pure $ statusCode == 200

submitPageUrlIO :: (MonadThrow m) => Task -> m URI
submitPageUrlIO (Task (ContestId cid) (ProblemId pid)) =
  mkURI $
    "https://atcoder.jp/contests/"
      <> cid
      <> "/submit?taskScreenName="
      <> dashToUnderscore cid
      <> "_"
      <> pid

-- --- Page Fetching Helpers ---
fetchTaskPage :: (MonadIO m, MonadReq m, HasLogger m) => AtCoderEnv -> ContestId -> m (Either AppError String)
fetchTaskPage _env (ContestId cid) = do
  let url = "https://atcoder.jp/contests/" <> cid <> "/tasks"
  logInfo $ "Fetching problems from: " <> url
  getHtml url

fetchProblemPage :: (MonadIO m, MonadReq m, HasLogger m) => AtCoderEnv -> Task -> m (Either AppError String)
fetchProblemPage _env (Task (ContestId cid) (ProblemId pid)) = do
  let url =
        "https://atcoder.jp/contests/"
          <> cid
          <> "/tasks/"
          <> dashToUnderscore cid
          <> "_"
          <> pid
  logInfo $ "[Skeleton] Fetching test cases from: " <> url
  getHtml url

-- --- Parsing Helpers ---
parseProblemIds :: (MonadIO m, HasLogger m) => String -> m (Either AppError [ProblemId])
parseProblemIds html = do
  logInfo "Parsing HTML response for problem ids..."

  either (pure . Left) logResult
    $ traverse validateProblemId
      . List.nub
      . map extractId
      . filter isTaskLink
    $ parseTags html
  where
    logResult :: (HasLogger m) => [ProblemId] -> m (Either AppError [ProblemId])
    logResult pid = do
      logInfo $ show (length pid) <> " problem ids found"
      return $ Right pid

    isTaskLink :: Tag String -> Bool
    isTaskLink (TagOpen "a" attrs) =
      case lookup "href" attrs of
        Just href -> "/contests/" `isPrefixOf` href && "/tasks/" `isInfixOf` href
        Nothing -> False
    isTaskLink _ = False

    extractId :: Tag String -> String
    extractId (TagOpen "a" attrs) =
      case lookup "href" attrs of
        Just href -> takeWhileEnd (/= '_') href
        Nothing -> ""
    extractId _ = ""

parseTestCases :: (MonadIO m, HasLogger m) => String -> m (Either AppError [TestCase])
parseTestCases body = do
  logInfo "Parsing HTML response for test cases..."
  either (pure . Left) logResult (go body)
  where
    logResult :: (HasLogger m) => [TestCase] -> m (Either AppError [TestCase])
    logResult tcs = do
      logInfo $ show (length tcs) <> " test cases found"
      return $ Right tcs

    go :: String -> Either AppError [TestCase]
    go html =
      pairUp (parseSamples html)
        <&> map mkTestCase . zipPairs

    mkTestCase :: (Int, (String, String)) -> TestCase
    mkTestCase (i, (input, output)) =
      TestCase
        { tcName = show i,
          tcInput = input,
          tcOutput = output
        }

    pairUp :: [a] -> Either AppError [(a, a)]
    pairUp [] = Right []
    pairUp (x : y : rest) = ((x, y) :) <$> pairUp rest
    pairUp _ = Left $ ProviderError "pairUp: odd length list"

    zipPairs :: [(a, a)] -> [(Int, (a, a))]
    zipPairs = zip [1 ..]

    parseSamples :: String -> [String]
    parseSamples html =
      go' (parseTags html) []
      where
        go' [] acc = acc
        go' (TagOpen "h3" _ : TagText heading : TagClose "h3" : rest) acc
          | "入力例" `isInfixOf` heading || "出力例" `isInfixOf` heading =
              case extractPre rest of
                (Just content, rest') -> go' rest' (acc ++ [content])
                (Nothing, rest') -> go' rest' acc
        go' (_ : xs) acc = go' xs acc

        extractPre (TagOpen "pre" _ : TagText content : TagClose "pre" : rest) =
          (Just content, rest)
        extractPre (_x : xs) = extractPre xs
        extractPre _ = (Nothing, [])