module Usecase.Login (login) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
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
  m (Either AppError ())
login = runExceptT $ do
  lift $ logInfo "Login..."

  session <- lift loadSession
  case session of
    Left SessionNotFound -> doLogin
    Left (InvalidSession _) -> doLogin
    Left e -> ExceptT $ return $ Left e
    Right s -> do
      isValid <- ExceptT (verifySession s)

      if isValid
        then lift $ sendMsg "すでにログインしています"
        else do
          lift $ sendMsg "現在保存されているセッション情報は古いため再度ログインを実行します..."
          doLogin
  where
    doLogin :: (HasStdin m, HasUser m, HasLogger m, HasSession m, HasAtcoder m) => ExceptT AppError m ()
    doLogin = do
      lift $ logInfo "Ask Session via stdin"
      lift $ sendMsg "ブラウザでAtcoderにログインしてCookieに保存されているREVEL_SESSIONを入力してください"
      session <- ExceptT askSession

      isValid <- ExceptT $ verifySession session

      when isValid $ ExceptT (saveSession session)

      if isValid
        then lift $ sendMsg "ログインに成功しました"
        else lift $ sendMsg "ログインに失敗しました"

    askSession :: (HasStdin m) => m (Either AppError Session)
    askSession = validateSession <$> readLine