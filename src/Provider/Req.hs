{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Provider.Req
  ( getHtmlIO,
    reqGetIO,
    reqGetWithSessionIO,
  )
where

import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import qualified Data.ByteString.Char8 as BSC
import Data.Functor ((<&>))
import Data.Proxy (Proxy)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TEnc
import qualified Data.Text.IO as TIO
import Interface (HasSession (loadSession))
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
import Provider.Utils (maybeToExceptT, try)
import Text.URI (mkURI)
import Types

getHtmlIO :: (HasSession m, MonadIO m, MonadCatch m) => String -> m (Either AppError Text)
getHtmlIO url =
  reqGetIO url bsResponse defaultHttpConfig mempty
    <&> fmap (TEnc.decodeUtf8 . responseBody)

reqGetIO ::
  (HasSession m, MonadIO m, MonadCatch m, HttpResponse r) =>
  String ->
  Proxy r ->
  HttpConfig ->
  Option Https ->
  m (Either AppError r)
reqGetIO urlStr proxy config option = do
  sess <- loadSession
  case sess of
    Left SessionNotFound ->
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
  (MonadIO m, MonadCatch m, HttpResponse r) =>
  String ->
  Proxy r ->
  HttpConfig ->
  Option Https ->
  m (Either AppError r)
reqGetWithoutSessionIO urlStr proxy config option = runExceptT $ do
  liftIO $ TIO.putStrLn $ ">>> Performing HTTP GET Request: " <> T.pack urlStr
  uri <- ExceptT $ try $ mkURI $ T.pack urlStr
  (url, urlOpt) <- maybeToExceptT (ProviderError "Invalid URI for req") $ useHttpsURI uri

  ExceptT $
    try $
      runReq config $ do
        req GET url NoReqBody proxy (urlOpt <> option)

reqGetWithSessionIO ::
  (MonadIO m, MonadCatch m, HttpResponse r) =>
  Session ->
  String ->
  Proxy r ->
  HttpConfig ->
  Option Https ->
  m (Either AppError r)
reqGetWithSessionIO (Session session) urlStr proxy config option = runExceptT $ do
  liftIO $ TIO.putStrLn $ ">>> Performing HTTP GET Request: " <> T.pack urlStr
  let cookieHeader = header "Cookie" (BSC.pack $ "REVEL_SESSION=" <> T.unpack session)
  uri <- ExceptT $ try $ mkURI $ T.pack urlStr
  (url, urlOpt) <- maybeToExceptT (ProviderError "Invalid URI for req") $ useHttpsURI uri

  ExceptT $
    try $
      runReq config $ do
        req GET url NoReqBody proxy (urlOpt <> option <> cookieHeader)