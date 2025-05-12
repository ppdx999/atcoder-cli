-- test/Usecase/LoginSpec.hs
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Usecase.LoginSpec (spec) where

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
              { msLoadSession = Right validSession,
                msVerifySession = [Right True]
              }
      -- Act
      (result, finalState) <- execMockApp login initialState
      -- Assert
      result `shouldBe` Right ()
      last (msSendMsg finalState) `shouldBe` "すでにログインしています"
      msSaveSession finalState `shouldBe` []

    it "セッションファイルが存在しない場合、標準入力からセッションを受け取り、検証・保存する" $ do
      -- Arrange
      let initialState =
            initialMockState
              { msLoadSession = Left SessionNotFound,
                msVerifySession = [Right True],
                msReadLine = [validSessionValue]
              }
      -- Act
      (result, finalState) <- execMockApp login initialState
      -- Assert
      result `shouldBe` Right ()
      last (msSendMsg finalState) `shouldBe` "ログインに成功しました"
      msSaveSession finalState `shouldBe` [validSession]

    it "セッションファイルが存在しても、検証に失敗したら再度ログイン処理を試みる" $ do
      -- Arrange
      let initialState =
            initialMockState
              { msLoadSession = Right (Session "old session"),
                msVerifySession = [Right False, Right True],
                msReadLine = [validSessionValue]
              }
      -- Act
      (result, finalState) <- execMockApp login initialState
      -- Assert
      result `shouldBe` Right ()
      last (msSendMsg finalState) `shouldBe` "ログインに成功しました"
      msSaveSession finalState `shouldBe` [validSession]

    it "標準入力から受け取ったセッションで検証に失敗したらログイン失敗と表示される" $ do
      -- Arrange
      let initialState =
            initialMockState
              { msLoadSession = Left SessionNotFound,
                msVerifySession = [Right False],
                msReadLine = ["invalid session"]
              }
      -- Act
      (result, finalState) <- execMockApp login initialState
      -- Assert
      result `shouldBe` Right ()
      last (msSendMsg finalState) `shouldBe` "ログインに失敗しました"
      msSaveSession finalState `shouldBe` []