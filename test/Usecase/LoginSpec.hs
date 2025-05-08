-- test/Usecase/LoginSpec.hs
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Usecase.LoginSpec (spec) where

import Control.Monad.Trans.Except (runExceptT)
import Mock
import Test.Hspec
import Types
import Usecase.Login (login)

spec :: Spec
spec = describe "Usecase.Login" $ do
  let validSessionValue = "valid_revel_session_cookie_value"
  let validSession = Session validSessionValue

  describe "login" $ do
    it "既存の有効なセッションがあれば、ログイン成功と表示する" $ do
      -- Arrange
      let initialState =
            initialMockState
              { msLoadSessionResult = Right validSession,
                msVerifySessionResultsQueue = [Right True]
              }
      -- Act
      (result, finalState) <- execMockApp (runExceptT login) initialState
      -- Assert
      result `shouldBe` Right ()
      last (msMsgs finalState) `shouldBe` "すでにログインしています"

    it "セッションファイルが存在しない場合、標準入力からセッションを受け取り、検証・保存する" $ do
      -- Arrange
      let initialState =
            initialMockState
              { msLoadSessionResult = Left SessionNotFound,
                msSaveSessionResult = \_ -> Right (),
                msVerifySessionResultsQueue = [Right True],
                msStdinQueue = [validSessionValue]
              }
      -- Act
      (result, finalState) <- execMockApp (runExceptT login) initialState
      -- Assert
      result `shouldBe` Right ()
      last (msMsgs finalState) `shouldBe` "ログインに成功しました"
      msSavedSessions finalState `shouldBe` [validSession]

    it "セッションファイルが存在しても、検証に失敗したら再度ログイン処理を試みる" $ do
      -- Arrange
      let initialState =
            initialMockState
              { msLoadSessionResult = Right (Session "old session"),
                msSaveSessionResult = \_ -> Right (),
                msVerifySessionResultsQueue = [Right False, Right True],
                msStdinQueue = [validSessionValue]
              }
      -- Act
      (result, finalState) <- execMockApp (runExceptT login) initialState
      -- Assert
      result `shouldBe` Right ()
      last (msMsgs finalState) `shouldBe` "ログインに成功しました"
      msSavedSessions finalState `shouldBe` [validSession]

    it "標準入力から受け取ったセッションで検証に失敗したらログイン失敗と表示される" $ do
      -- Arrange
      let initialState =
            initialMockState
              { msLoadSessionResult = Left SessionNotFound,
                msVerifySessionResultsQueue = [Right False],
                msStdinQueue = ["invalid session"]
              }
      -- Act
      (result, finalState) <- execMockApp (runExceptT login) initialState
      -- Assert
      result `shouldBe` Right ()
      last (msMsgs finalState) `shouldBe` "ログインに失敗しました"