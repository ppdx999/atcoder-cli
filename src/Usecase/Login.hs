{-# LANGUAGE OverloadedStrings #-}

module Usecase.Login (login) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Except (ExceptT (ExceptT))
import qualified Data.Text as T
import Interface
import Types (AppError (..), Session (..), validateSession)

login ::
  ( MonadIO m,
    HasLogger m,
    HasStdin m,
    HasAtcoder m,
    HasSession m,
    HasUser m
  ) =>
  ExceptT AppError m ()
login = do
  logInfoE "Load Session"
  session <- lift loadSession
  case session of
    Left SessionNotFound -> doLogin
    Left (InvalidSession _) -> doLogin
    Left e -> ExceptT $ return $ Left e
    Right s -> do
      logInfoE "Verify Session..."
      isValid <- ExceptT (verifySession s)

      if isValid
        then sendMsgE "すでにログインしています"
        else do
          sendMsgE "現在保存されているセッション情報は古いため再度ログインを実行します..."
          doLogin
  where
    doLogin :: (HasStdin m, HasUser m, HasLogger m, HasSession m, HasAtcoder m) => ExceptT AppError m ()
    doLogin = do
      logInfoE "Ask Session via stdin"
      sendMsgE "ブラウザでAtcoderにログインしてREVEL_SESSIONを入力してください"
      session <- ExceptT askSession

      logInfoE "Verify Session..."
      isValid <- ExceptT $ verifySession session

      logInfoE $ "This Session is " <> showBool isValid
      when isValid $ ExceptT (saveSession session)

      if isValid
        then sendMsgE "ログインに成功しました"
        else sendMsgE "ログインに失敗しました"

    logInfoE :: (HasLogger m, MonadTrans t) => T.Text -> t m ()
    logInfoE = lift . logInfo

    sendMsgE :: (HasUser m, MonadTrans t) => T.Text -> t m ()
    sendMsgE = lift . sendMsg

    askSession :: (HasStdin m) => m (Either AppError Session)
    askSession = validateSession <$> readLine

    showBool :: Bool -> T.Text
    showBool = T.pack . show