{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Provider.Req
  ( getHtmlIO,
    reqGetIO,
    reqGetWithSessionIO,
  )
where

import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (MonadTrans (lift))
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import qualified Data.ByteString.Char8 as BSC
import Data.Functor ((<&>))
import Data.Proxy (Proxy)
import Interface (HasLogger (logInfo), HasSession (loadSession))
import Network.HTTP.Req
  ( GET (GET),
    HttpConfig,
    HttpResponse,
    NoReqBody (NoReqBody),
    Option,
    Scheme (Https),
    bsResponse,
    defaultHttpConfig,
    header,
    req,
    responseBody,
    runReq,
    useHttpsURI,
  )
import Provider.Utils (crlfToLf, decodeUtf8, maybeToExceptT, mkURI, try)
import Types

normalizeHtml :: String -> String
normalizeHtml = crlfToLf

getHtmlIO :: (HasSession m, HasLogger m, MonadIO m, MonadCatch m) => String -> m (Either AppError String)
getHtmlIO url =
  reqGetIO url bsResponse defaultHttpConfig mempty
    <&> fmap (normalizeHtml . decodeUtf8 . responseBody)

reqGetIO ::
  (HasSession m, HasLogger m, MonadIO m, MonadCatch m, HttpResponse r) =>
  String ->
  Proxy r ->
  HttpConfig ->
  Option Https ->
  m (Either AppError r)
reqGetIO urlStr proxy config option = do
  sess <- loadSession
  case sess of
    Left SessionNotFound -> do
      reqGetWithoutSessionIO
        urlStr
        proxy
        config
        option
    Left e -> pure $ Left e
    Right session ->
      reqGetWithSessionIO
        session
        urlStr
        proxy
        config
        option

reqGetWithoutSessionIO ::
  (MonadIO m, HasLogger m, MonadCatch m, HttpResponse r) =>
  String ->
  Proxy r ->
  HttpConfig ->
  Option Https ->
  m (Either AppError r)
reqGetWithoutSessionIO urlStr proxy config option = runExceptT $ do
  lift $ logInfo $ ">>> Performing HTTP GET Request without Session: " <> urlStr
  uri <- ExceptT $ try $ mkURI urlStr
  (url, urlOpt) <- maybeToExceptT (ProviderError "Invalid URI for req") $ useHttpsURI uri

  ExceptT $
    try $
      runReq config $ do
        req GET url NoReqBody proxy (urlOpt <> option)

reqGetWithSessionIO ::
  (MonadIO m, HasLogger m, MonadCatch m, HttpResponse r) =>
  Session ->
  String ->
  Proxy r ->
  HttpConfig ->
  Option Https ->
  m (Either AppError r)
reqGetWithSessionIO (Session session) urlStr proxy config option = runExceptT $ do
  lift $ logInfo $ ">>> Performing HTTP GET Request with session: " <> urlStr
  let cookieHeader = header (BSC.pack "Cookie") (BSC.pack $ "REVEL_SESSION=" <> session)
  uri <- ExceptT $ try $ mkURI urlStr
  (url, urlOpt) <- maybeToExceptT (ProviderError "Invalid URI for req") $ useHttpsURI uri

  ExceptT $
    try $
      runReq config $ do
        req GET url NoReqBody proxy (urlOpt <> option <> cookieHeader)