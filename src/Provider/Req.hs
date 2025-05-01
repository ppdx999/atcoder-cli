{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Provider.Req
  ( reqGetIO,
  )
where

import Control.Monad.Catch (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Network.HTTP.Req
  ( GET (GET),
    NoReqBody (NoReqBody),
    Option,
    Scheme (Http, Https),
    Url,
    bsResponse,
    defaultHttpConfig,
    req,
    responseBody,
    runReq,
    useURI,
  )
import Text.URI (mkURI)
import Types

reqGetIO :: String -> IO (Either AppError BSC.ByteString)
reqGetIO url = runExceptT $ do
  liftIO $ TIO.putStrLn $ ">>> Performing HTTP GET (req): " <> T.pack url
  parseAndValidateUrl url >>= tryHttpRequest

-- | URL 文字列をパースし、Http または Https の URL とオプションに変換する。
--   失敗した場合は ExceptT 内で AppError を投げる。
parseAndValidateUrl :: String -> ExceptT AppError IO (Either (Url Http, Option Http) (Url Https, Option Https))
parseAndValidateUrl urlStr = do
  uri <-
    maybeToExceptT
      (ProviderError $ "Failed to create URI from: " <> T.pack urlStr)
      (mkURI $ T.pack urlStr)
  liftIO $ TIO.putStrLn $ ">>> URI created: " <> T.pack (show uri)
  parsedResult <-
    maybeToExceptT
      (ProviderError $ "Invalid scheme or format for URL: " <> T.pack urlStr)
      (useURI uri)
  liftIO $ TIO.putStrLn ">>> URI parsed successfully (Http or Https)."
  pure parsedResult

-- | パースされた URL を使って HTTP GET リクエストを実行し、レスポンスボディを返す。
--   HTTP 通信中の例外を捕捉し、AppError として ExceptT 内で投げる。
tryHttpRequest :: Either (Url Http, Option Http) (Url Https, Option Https) -> ExceptT AppError IO BSC.ByteString
tryHttpRequest parsedUrl = do
  liftIO $ TIO.putStrLn ">>> Attempting HTTP request..."
  eResp <- liftIO $ try $ runReq defaultHttpConfig $ do
    case parsedUrl of
      Left (reqUrl, options) -> responseBody <$> req GET reqUrl NoReqBody bsResponse options
      Right (reqUrl, options) -> responseBody <$> req GET reqUrl NoReqBody bsResponse options
  either handleError handleSuccess eResp
  where
    handleError e = do
      liftIO $ TIO.putStrLn $ ">>> Request failed: " <> T.pack (show e)
      throwE $ ProviderError $ T.pack $ "HTTP Request failed: " <> show (e :: SomeException)
    handleSuccess body = do
      liftIO $ TIO.putStrLn ">>> Request successful."
      pure body

maybeToExceptT :: (Monad m) => e -> Maybe a -> ExceptT e m a
maybeToExceptT err = maybe (throwE err) pure