module Usecase.Test (test) where

import Control.Monad.Trans (MonadTrans (lift))
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import Data.Foldable (traverse_)
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

  lang <- ExceptT detectLanguage

  ExceptT $ buildLanguage lang

  tcs <- ExceptT loadTestCases

  results <- traverse (ExceptT . runTestCase lang) tcs

  traverse_ (ExceptT . reportTestResult) $ zip tcs results

  ExceptT $ cleanupBuiltFile lang