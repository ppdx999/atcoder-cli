-- src/Provider/Atcoder.hs
{-# LANGUAGE OverloadedStrings #-}

module Provider.Atcoder
  ( -- 現状のダミー実装 (テストや初期開発で使うかも)
    fetchProblemIdsDummyIO,
    fetchTestCasesDummyIO,
    -- 実際の IO 実装 (これから実装)
    fetchProblemIdsIO,
    fetchTestCasesIO,
    -- 実際の処理に必要な環境 (req用に調整、必要なら変更)
    AtCoderEnv (..),
    createAtCoderEnv,
  )
where

-- 例外処理のために保持

-- import qualified Data.ByteString.Lazy as LBS -- req は Strict ByteString を主に使うかも

-- import Network.HTTP.Req -- req ライブラリをインポート (あとで追加)
-- import Text.Regex.TDFA -- 正規表現ライブラリをインポート (あとで追加)

import Control.Exception (IOException) -- IOException はネットワークエラーなどで発生しうる
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Types

-- | AtCoder との通信に必要な環境 (req 用に調整、シンプルに)
--   req は Manager を明示的に管理しないことが多いので削除。
--   将来的にクッキーなどが必要ならここに追加。
data AtCoderEnv = AtCoderEnv
  {
  }
  -- acSessionCookie :: Maybe SessionCookie -- 例: セッションクッキー
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
fetchProblemIdsIO :: (MonadIO m, MonadThrow m) => AtCoderEnv -> ContestId -> m (Either AppError [ProblemId])
fetchProblemIdsIO _env contestId = do
  let url = "https://atcoder.jp/contests/" <> T.unpack (deContestId contestId) <> "/tasks"
  liftIO $ TIO.putStrLn $ "[Skeleton] Fetching problems from: " <> T.pack url

  -- ここで req を使って HTTP GET リクエストを送信する
  -- 例: response <- liftIO $ try $ runReq defaultHttpConfig $ do ... req GET ...
  --     case response of
  --       Left e -> pure $ Left (ProviderError (T.pack $ show (e :: SomeException))) -- req の例外型に合わせる
  --       Right httpResponse -> do
  --         let body = responseBody httpResponse -- req のレスポンスボディ取得方法に合わせる
  --         liftIO $ TIO.putStrLn "[Skeleton] Parsing HTML response for problems using Regex..."
  --         -- ここで正規表現を使って ProblemId を抽出する
  --         -- pure $ parseProblemIdsWithRegex body
  --         pure $ Left (ProviderError "Problem ID parsing (Regex) not implemented yet.")

  -- TDD のための仮実装
  pure $ Left (ProviderError "fetchProblemIdsIO not implemented yet.")

-- | Real IO implementation for fetchTestCases (骨格 - req + Regex 想定)
fetchTestCasesIO :: (MonadIO m, MonadThrow m) => AtCoderEnv -> Task -> m (Either AppError [TestCase])
fetchTestCasesIO _env task = do
  let contest = deContestId (taskContestId task)
  let problem = deProblemId (taskProblemId task)
  let url = "https://atcoder.jp/contests/" <> T.unpack contest <> "/tasks/" <> T.unpack contest <> "_" <> T.unpack problem
  liftIO $ TIO.putStrLn $ "[Skeleton] Fetching test cases from: " <> T.pack url

  -- ここで req を使って HTTP GET リクエストを送信する
  -- 例: response <- liftIO $ try $ runReq defaultHttpConfig $ do ... req GET ...
  --     case response of
  --       Left e -> pure $ Left (ProviderError (T.pack $ show (e :: SomeException)))
  --       Right httpResponse -> do
  --         let body = responseBody httpResponse
  --         liftIO $ TIO.putStrLn "[Skeleton] Parsing HTML response for test cases using Regex..."
  --         -- ここで正規表現を使って TestCase を抽出する
  --         -- pure $ parseTestCasesWithRegex body
  --         pure $ Left (ProviderError "Test case parsing (Regex) not implemented yet.")

  -- TDD のための仮実装
  pure $ Left (ProviderError "fetchTestCasesIO not implemented yet.")

-- --- Regex Parsing Helpers (未実装 - 必要に応じて追加) ---

-- parseProblemIdsWithRegex :: ResponseBody -> Either AppError [ProblemId]
-- parseProblemIdsWithRegex body = ...

-- parseTestCasesWithRegex :: ResponseBody -> Either AppError [TestCase]
-- parseTestCasesWithRegex body = ...
