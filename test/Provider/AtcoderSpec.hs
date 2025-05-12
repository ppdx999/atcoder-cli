-- test/Provider/AtcoderSpec.hs
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Provider.AtcoderSpec (spec) where

import Mock
import Provider.Atcoder
import Test.Hspec
import TestUtils (isProviderError)
import Types

spec :: Spec
spec = describe "Provider.Atcoder" $ do
  describe "fetchProblemIdsIO" $ do
    let contest = ContestId "abc100"
    it "正常なHTMLを受け取った場合、ProblemIdのリストを返す" $ do
      -- 1. Arrange: モックの準備
      let html =
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
      let initialState =
            initialMockState
              { -- Mock MonadReq
                msGetHtml = Right html
              }

      -- 2. Act: テスト対象関数の実行
      (result, _finalState) <- execMockApp (fetchProblemIdsIO contest) initialState

      -- 3. Assert: 結果の検証
      result `shouldBe` Right [ProblemId "a", ProblemId "b", ProblemId "c"]

    it "HTTPリクエストが失敗した場合、ProviderErrorを返す" $ do
      -- 1. Arrange
      let initialState =
            initialMockState
              { -- Mock MonadReq
                msGetHtml = Left $ ProviderError "Simulated Network Error"
              }

      -- 2. Act
      (result, _finalState) <- execMockApp (fetchProblemIdsIO contest) initialState

      -- 3. Assert
      result `shouldSatisfy` isProviderError

  describe "fetchTestCasesIO" $ do
    let task = Task (ContestId "abc100") (ProblemId "a")
    it "正常なHTMLを受け取った場合、TestCaseのリストを返す" $ do
      -- 1. Arrange
      let html =
            unlines
              [ "<html><body>",
                "<h3>入力例 1</h3>",
                "<pre>1 2",
                "</pre>",
                "<h3>出力例 1</h3>",
                "<pre>3",
                "</pre>",
                "<h3>入力例 2</h3>",
                "<pre>10 20",
                "</pre>",
                "<h3>出力例 2</h3>",
                "<pre>30",
                "20",
                "</pre>",
                "<h3>Sample 1</h3>",
                "<pre>",
                "</pre>", -- そのほかにPreが来ても無視される
                "</body></html>"
              ]
      let initialState =
            initialMockState
              { -- Mock MonadReq
                msGetHtml = Right html
              }

      -- 2. Act
      (result, _finalState) <- execMockApp (fetchTestCasesIO task) initialState

      -- 3. Assert
      result
        `shouldBe` Right
          [ TestCase {tcName = "1", tcInput = "1 2\n", tcOutput = "3\n"},
            TestCase {tcName = "2", tcInput = "10 20\n", tcOutput = "30\n20\n"}
          ]

    it "HTTPリクエストが失敗した場合、ProviderErrorを返す" $ do
      -- 1. Arrange
      let initialState =
            initialMockState
              { -- Mock MonadReq
                msGetHtml = Left $ ProviderError "Simulated Network Error"
              }

      -- 2. Act
      (result, _finalState) <- execMockApp (fetchTestCasesIO task) initialState

      -- 3. Assert
      result `shouldSatisfy` isProviderError

    it "HTMLのパースに失敗した場合（サンプルが見つからない等）、エラーを返す" $ do
      -- 1. Arrange
      let html = "<html><body>No samples here</body></html>"
      let initialState =
            initialMockState
              { -- Mock MonadReq
                msGetHtml = Right html
              }

      -- 2. Act
      (result, _finalState) <- execMockApp (fetchTestCasesIO task) initialState

      -- 3. Assert
      result `shouldBe` Right []

    it "HTMLのパースに失敗した場合（入力と出力のペアが奇数）、エラーを返す" $ do
      -- 1. Arrange
      let html = unlines ["<html><body>", "<h3>入力例 1</h3>", "<pre>1 2</pre>", "</body></html>"]
      let initialState =
            initialMockState
              { -- Mock MonadReq
                msGetHtml = Right html
              }

      -- 2. Act
      (result, _finalState) <- execMockApp (fetchTestCasesIO task) initialState

      -- 3. Assert
      result `shouldSatisfy` isProviderError