{-# LANGUAGE OverloadedStrings #-}

module Usecase.Test (test) where

import Control.Monad.Trans (MonadTrans (lift))
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import Data.Foldable (traverse_)
import qualified Data.Text as T
import Interface (HasLanguage (..), HasLogger (..), HasReporter (..), HasTestCase (..))
import Types

test ::
  ( HasLogger m,
    HasLanguage m,
    HasTestCase m,
    HasReporter m
  ) =>
  m (Either AppError ())
test = runExceptT $ do
  logInfoE "Testing ..."

  logInfoE "Detecting language ..."
  lang <- ExceptT detectLanguage
  logInfoE $ "Detected language: " <> langName lang

  logInfoE "Building language ..."
  ExceptT $ buildLanguage lang

  logInfoE "Loading test cases ..."
  tcs <- ExceptT loadTestCases
  logInfoE $ "Found " <> T.pack (show $ length tcs) <> " test cases."

  logInfoE "Running test cases ..."
  traverse_ (ExceptT . runAndReport lang) tcs

  logInfoE "Cleaning up build files ..."
  ExceptT $ cleanupBuiltFile lang
  where
    logInfoE :: (HasLogger m, MonadTrans t) => T.Text -> t m ()
    logInfoE = lift . logInfo

    runAndReport :: (HasLanguage m, HasReporter m) => Language -> TestCase -> m (Either AppError ())
    runAndReport lang tc =
      runTestCase lang tc
        >>= either (return . Left) (report tc)