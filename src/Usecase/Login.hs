{-# LANGUAGE OverloadedStrings #-}

module Usecase.Login (login) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Except (ExceptT (ExceptT))
import qualified Data.Text as T
import Interface
import Types (AppError (..), Config (sessionPath), Session (..), validateSession)

login ::
  ( MonadIO m,
    HasLogger m,
    HasStdin m,
    HasFileSystem m,
    HasAtcoder m,
    HasConfig m
  ) =>
  ExceptT AppError m ()
login = do
  logInfoE "Load Configuration..."
  config <- lift getConfig

  logInfoE "Load Session"
  session <- lift $ loadSession (sessionPath config)

  case session of
    Left SessionNotFound -> doLogin config
    Left e -> ExceptT $ return $ Left e
    Right s -> do
      logInfoE "Verify Session..."
      isValid <- ExceptT (verifySession s)

      if isValid
        then logInfoE "すでにログインしています"
        else do
          logInfoE "現在保存されているセッション情報は古いため再度ログインを実行します..."
          doLogin config
  where
    doLogin :: (HasStdin m, HasLogger m, HasFileSystem m, HasAtcoder m) => Config -> ExceptT AppError m ()
    doLogin config = do
      logInfoE "Ask Session via stdin"
      session <- ExceptT askSession

      logInfoE "Verify Session..."
      isValid <- ExceptT $ verifySession session

      logInfoE $ "This Session is " <> showBool isValid
      when isValid $ ExceptT (saveSession (sessionPath config) session)

      if isValid
        then logInfoE "ログインに成功しました"
        else logInfoE "ログインに失敗しました"

    logInfoE :: (HasLogger m, MonadTrans t) => T.Text -> t m ()
    logInfoE = lift . logInfo

    askSession :: (HasStdin m) => m (Either AppError Session)
    askSession = validateSession <$> readLine

    showBool :: Bool -> T.Text
    showBool = T.pack . show