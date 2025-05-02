-- test/Provider/AtcoderSpec.hs
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Provider.AtcoderSpec (spec) where

import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Identity (Identity (..))
import Control.Monad.State.Strict (MonadState, StateT (..), evalStateT, get, gets, modify)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Interface (MonadReq (..))
import Provider.Atcoder
import Test.Hspec
import Types

data MockReqState = MockReqState
  { mockResponses :: Map.Map String (Either AppError BSC.ByteString),
    mockLogs :: [T.Text]
  }

newtype MockReq a = MockReq {runMockReq :: StateT MockReqState Identity a}
  deriving (Functor, Applicative, Monad, MonadState MockReqState)

instance MonadIO MockReq where
  liftIO _ = do
    modify (\s -> s {mockLogs = mockLogs s <> ["liftIO called"]})
    pure (error "Dummy value for liftIO")

instance MonadThrow MockReq where
  throwM e = error $ "MonadThrow MockReq: Unexpected throwM: " ++ show e

instance MonadReq MockReq where
  reqGet url = do
    responses <- gets mockResponses
    case Map.lookup url responses of
      Just response -> pure response
      Nothing -> pure $ Left (ProviderError $ "MockReq: No response defined for URL: " <> T.pack url)

evalMockReq :: MockReq a -> MockReqState -> a
evalMockReq action state = runIdentity (evalStateT (runMockReq action) state)

testEnv :: AtCoderEnv
testEnv = AtCoderEnv {}

spec :: Spec
spec = describe "Provider.Atcoder" $ do
  let unsafeGetRight (Right x) = x
      unsafeGetRight (Left e) = error ("Test setup failed: " ++ show e)
  describe "fetchProblemIdsIO" $ do
    it "正常なHTMLを受け取った場合、ProblemIdのリストを返す" $ do
      -- 1. Arrange: モックの準備
      let contest = unsafeGetRight $ toContestId "abc100"
      let url = "https://atcoder.jp/contests/abc100/tasks"
      -- AtCoder の実際の HTML 構造に近いダミーデータ (正規表現で抽出できる形式)
      let dummyHtml =
            BSC.pack $
              unlines
                [ "<html><body>",
                  "<h1>Tasks</h1>",
                  "<table><tbody>",
                  "<tr><td>A</td><td><a href='/contests/abc100/tasks/abc100_a'>Problem A</a></td></tr>",
                  "<tr><td>B</td><td><a href='/contests/abc100/tasks/abc100_b'>Problem B</a></td></tr>",
                  "<tr><td>C</td><td><a href='/contests/abc100/tasks/abc100_c'>Problem C</a></td></tr>",
                  "</tbody></table>",
                  "</body></html>"
                ]
      let responses = Map.singleton url (Right dummyHtml)
      let expectedProblemA = unsafeGetRight $ toProblemId "a"
      let expectedProblemB = unsafeGetRight $ toProblemId "b"
      let expectedProblemC = unsafeGetRight $ toProblemId "c"
      let expected = Right [expectedProblemA, expectedProblemB, expectedProblemC]
      let initialMockState =
            MockReqState
              { mockResponses = responses,
                mockLogs = []
              }

      -- 2. Act: テスト対象関数の実行
      let result = evalMockReq (fetchProblemIdsIO testEnv contest) initialMockState

      -- 3. Assert: 結果の検証
      result `shouldBe` expected
    it "HTTPリクエストが失敗した場合、ProviderErrorを返す" $ do
      let unsafeGetRight (Right x) = x
          unsafeGetRight (Left e) = error ("Test setup failed: " ++ show e)
      -- 1. Arrange
      let contest = unsafeGetRight $ toContestId "abc100"
      let url = "https://atcoder.jp/contests/abc100/tasks"
      let expectedError = ProviderError "Simulated Network Error"
      let responses = Map.singleton url (Left expectedError)
      let initialMockState =
            MockReqState
              { mockResponses = responses,
                mockLogs = []
              }

      -- 2. Act
      let result = evalMockReq (fetchProblemIdsIO testEnv contest) initialMockState

      -- 3. Assert
      result `shouldBe` Left expectedError

    it "HTMLのパースに失敗した場合（該当リンクが見つからない等）、エラーを返す" $ do
      pendingWith "parseProblemIdsWithRegex の実装が必要"

  describe "fetchTestCasesIO" $ do
    it "（未実装）正常なHTMLを受け取った場合、TestCaseのリストを返す" $ do
      pendingWith "HTTPモックと実装が必要"
    -- ... fetchTestCasesIO 用のテストケース ...

    it "（未実装）HTTPリクエストが失敗した場合、ProviderErrorを返す" $ do
      pendingWith "HTTPモックと実装が必要"
    -- ... fetchTestCasesIO 用のテストケース ...

    it "（未実装）HTMLのパースに失敗した場合（サンプルが見つからない等）、エラーを返す" $ do
      pendingWith "HTTPモックと実装が必要"
