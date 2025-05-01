-- test/Provider/AtcoderSpec.hs
{-# LANGUAGE OverloadedStrings #-}

module Provider.AtcoderSpec (spec) where

import Test.Hspec

-- import Provider.Atcoder -- これからテストするモジュール
-- import Types            -- 必要な型定義
-- import Control.Monad.IO.Class (liftIO) -- IO を使う場合
-- import qualified Data.ByteString.Lazy.Char8 as LBSC -- ダミーレスポンス用
-- import qualified Data.Text as T

spec :: Spec
spec = describe "Provider.Atcoder" $ do
  describe "fetchProblemIdsIO" $ do
    it "（未実装）正常なHTMLを受け取った場合、ProblemIdのリストを返す" $ do
      pendingWith "HTTPモックと実装が必要"
    -- let dummyHtml = LBSC.pack "<html>...<a>abc100_a</a>...<a>abc100_b</a>...</html>"
    -- let mockGetter url = pure $ Right dummyHtml -- モックされたHTTP GETアクション
    -- let env = AtCoderEnv {}
    -- contestId <- either error pure $ toContestId "abc100"
    -- result <- fetchProblemIdsIO mockGetter env contestId -- モックを注入
    -- expectedProblemA <- either error pure $ toProblemId "a"
    -- expectedProblemB <- either error pure $ toProblemId "b"
    -- result `shouldBe` Right [expectedProblemA, expectedProblemB]

    it "（未実装）HTTPリクエストが失敗した場合、ProviderErrorを返す" $ do
      pendingWith "HTTPモックと実装が必要"
    -- let mockGetter url = pure $ Left (ProviderError "Network Error Simulation") -- 失敗するモック
    -- let env = AtCoderEnv {}
    -- contestId <- either error pure $ toContestId "abc100"
    -- result <- fetchProblemIdsIO mockGetter env contestId
    -- result `shouldSatisfy` (\r -> case r of Left (ProviderError _) -> True; _ -> False)

    it "（未実装）HTMLのパースに失敗した場合（該当リンクが見つからない等）、エラーを返す" $ do
      pendingWith "HTTPモックと実装が必要"
  -- let dummyHtml = LBSC.pack "<html><body>No task links here</body></html>"
  -- let mockGetter url = pure $ Right dummyHtml
  -- let env = AtCoderEnv {}
  -- contestId <- either error pure $ toContestId "abc100"
  -- result <- fetchProblemIdsIO mockGetter env contestId
  -- result `shouldSatisfy` (\r -> case r of Left (ProviderError _) -> True; _ -> False) -- または別のエラー型

  describe "fetchTestCasesIO" $ do
    it "（未実装）正常なHTMLを受け取った場合、TestCaseのリストを返す" $ do
      pendingWith "HTTPモックと実装が必要"
    -- let dummyHtml = LBSC.pack "<html>...<pre id='sample1-input'>1 2</pre>...<pre id='sample1-output'>3</pre>...</html>"
    -- let mockGetter url = pure $ Right dummyHtml
    -- let env = AtCoderEnv {}
    -- contestId <- either error pure $ toContestId "abc100"
    -- problemId <- either error pure $ toProblemId "a"
    -- let task = Task contestId problemId
    -- result <- fetchTestCasesIO mockGetter env task
    -- let expectedCase = TestCase "sample1" "1 2" "3" -- 仮。ByteStringに要変換
    -- result `shouldBe` Right [expectedCase]

    it "（未実装）HTTPリクエストが失敗した場合、ProviderErrorを返す" $ do
      pendingWith "HTTPモックと実装が必要"
    -- let mockGetter url = pure $ Left (ProviderError "Network Error Simulation")
    -- let env = AtCoderEnv {}
    -- contestId <- either error pure $ toContestId "abc100"
    -- problemId <- either error pure $ toProblemId "a"
    -- let task = Task contestId problemId
    -- result <- fetchTestCasesIO mockGetter env task
    -- result `shouldSatisfy` (\r -> case r of Left (ProviderError _) -> True; _ -> False)

    it "（未実装）HTMLのパースに失敗した場合（サンプルが見つからない等）、エラーを返す" $ do
      pendingWith "HTTPモックと実装が必要"

-- let dummyHtml = LBSC.pack "<html><body>No samples here</body></html>"
-- let mockGetter url = pure $ Right dummyHtml
-- let env = AtCoderEnv {}
-- contestId <- either error pure $ toContestId "abc100"
-- problemId <- either error pure $ toProblemId "a"
-- let task = Task contestId problemId
-- result <- fetchTestCasesIO mockGetter env task
-- result `shouldSatisfy` (\r -> case r of Left (ProviderError _) -> True; _ -> False)
