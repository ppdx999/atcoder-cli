{-# LANGUAGE OverloadedStrings #-}

module Usecase.Test (test) where

import Control.Monad.Trans (MonadTrans (lift))
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import Data.Foldable (traverse_)
import qualified Data.Text as T
import Interface (HasLanguage (..), HasLogger (..), HasTestCase (..))
import Types

test ::
  ( HasLogger m,
    HasLanguage m,
    HasTestCase m
  ) =>
  m (Either AppError ())
test = runExceptT $ do
  lift $ logInfo "Testing ..."

  lift $ logInfo "Detecting language ..."
  lang <- ExceptT detectLanguage
  lift $ logInfo $ "Detected language: " <> langName lang

  lift $ logInfo "Building language ..."
  ExceptT $ buildLanguage lang

  lift $ logInfo "Loading test cases ..."
  tcs <- ExceptT loadTestCases
  lift $ logInfo $ "Found " <> T.pack (show $ length tcs) <> " test cases."

  lift $ logInfo "Running test cases ..."
  results <- traverse (ExceptT . runTestCase lang) tcs

  lift $ logInfo "Testcase Result ..."
  traverse_ (ExceptT . report) $ zip tcs results

  lift $ logInfo "Cleaning up build files ..."
  ExceptT $ cleanupBuiltFile lang