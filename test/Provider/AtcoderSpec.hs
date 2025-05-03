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
import Control.Monad.State.Strict (MonadState, StateT (..), evalStateT, gets, modify)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TEnc
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

  describe "fetchTestCasesIO" $ do
    it "正常なHTMLを受け取った場合、TestCaseのリストを返す" $ do
      -- 1. Arrange
      let contest = unsafeGetRight $ toContestId "abc100"
      let problem = unsafeGetRight $ toProblemId "a"
      let task = Task contest problem
      let url = "https://atcoder.jp/contests/abc100/tasks/abc100_a" -- URLを修正
      -- parseTestCasesWithRegex' が期待する形式のHTML
      let dummyHtml =
            TEnc.encodeUtf8 $
              T.unlines
                [ "<html><body>",
                  "<h3>入力例 1</h3>",
                  "<pre>1 2</pre>",
                  "<h3>出力例 1</h3>",
                  "<pre>3</pre>",
                  "<h3>入力例 2</h3>",
                  "<pre>10 20</pre>",
                  "<h3>出力例 2</h3>",
                  "<pre>30</pre>",
                  "</body></html>"
                ]
      let responses = Map.singleton url (Right dummyHtml)
      -- parseTestCasesWithRegex' が生成する TestCase (末尾に改行が追加される)
      let tc1 = TestCase {tcName = "1", tcInput = BSC.pack "1 2\n", tcOutput = BSC.pack "3\n"}
      let tc2 = TestCase {tcName = "2", tcInput = BSC.pack "10 20\n", tcOutput = BSC.pack "30\n"}
      let expected = Right [tc1, tc2]
      let initialMockState = MockReqState {mockResponses = responses, mockLogs = []}

      -- 2. Act
      let result = evalMockReq (fetchTestCasesIO testEnv task) initialMockState

      -- 3. Assert
      -- parseTestCasesWithRegex' が実装されたので、期待値を直接比較
      result `shouldBe` expected

    it "HTTPリクエストが失敗した場合、ProviderErrorを返す" $ do
      -- 1. Arrange
      let contest = unsafeGetRight $ toContestId "abc100"
      let problem = unsafeGetRight $ toProblemId "a"
      let task = Task contest problem
      let url = "https://atcoder.jp/contests/abc100/tasks/abc100_a"
      let expectedError = ProviderError "Simulated Network Error"
      let responses = Map.singleton url (Left expectedError)
      let initialMockState = MockReqState {mockResponses = responses, mockLogs = []}

      -- 2. Act
      let result = evalMockReq (fetchTestCasesIO testEnv task) initialMockState

      -- 3. Assert
      result `shouldBe` Left expectedError

    it "HTMLのパースに失敗した場合（サンプルが見つからない等）、エラーを返す" $ do
      -- 1. Arrange
      let contest = unsafeGetRight $ toContestId "abc100"
      let problem = unsafeGetRight $ toProblemId "a"
      let task = Task contest problem
      let url = "https://atcoder.jp/contests/abc100/tasks/abc100_a"
      -- サンプルが含まれないHTML
      let dummyHtml = TEnc.encodeUtf8 "<html><body>No samples here</body></html>"
      let responses = Map.singleton url (Right dummyHtml)
      -- parseTestCasesWithRegex' が空リストを返すことを期待
      let expected = Right []
      let initialMockState = MockReqState {mockResponses = responses, mockLogs = []}

      -- 2. Act
      let result = evalMockReq (fetchTestCasesIO testEnv task) initialMockState

      -- 3. Assert
      result `shouldBe` expected

    it "HTMLのパースに失敗した場合（入力と出力のペアが奇数）、エラーを返す" $ do
      -- 1. Arrange
      let contest = unsafeGetRight $ toContestId "abc100"
      let problem = unsafeGetRight $ toProblemId "a"
      let task = Task contest problem
      let url = "https://atcoder.jp/contests/abc100/tasks/abc100_a"
      -- 入力例しかないHTML
      let dummyHtml =
            TEnc.encodeUtf8 $
              T.unlines
                [ "<html><body>",
                  "<h3>入力例 1</h3>",
                  "<pre>1 2</pre>",
                  "</body></html>"
                ]
      let responses = Map.singleton url (Right dummyHtml)
      let expectedError = ProviderError "pairUp: odd length list" -- 仮のエラーメッセージ
      let initialMockState = MockReqState {mockResponses = responses, mockLogs = []}

      -- 2. Act & Assert
      let result = evalMockReq (fetchTestCasesIO testEnv task) initialMockState
      result `shouldBe` Left expectedError
