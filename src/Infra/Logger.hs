-- src/Infrastructure/Logger.hs
{-# LANGUAGE OverloadedStrings #-}

module Infra.Logger
  ( logInfoIO,
    logErrorIO,
  )
where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- | IO implementation for logInfo
logInfoIO :: T.Text -> IO ()
logInfoIO = TIO.putStrLn

-- | IO implementation for logError (e.g., add a prefix)
logErrorIO :: T.Text -> IO ()
logErrorIO msg = TIO.putStrLn $ "[ERROR] " <> msg
