module Usecase.Test (test) where

import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import Data.Foldable (traverse_)
import Interface (HasExecutor (..), HasLanguage (detectLanguage), HasLogger (..), HasReporter (..), HasTestCase (..))
import Types

test ::
  ( HasLogger m,
    HasLanguage m,
    HasTestCase m,
    HasExecutor m,
    HasReporter m
  ) =>
  m (Either AppError ())
test = runExceptT $ do
  lang <- ExceptT detectLanguage
  tcs <- ExceptT loadTestCases
  traverse_ (ExceptT . runAndReport lang) tcs
  where
    runAndReport :: (HasExecutor m, HasReporter m) => Language -> TestCase -> m (Either AppError ())
    runAndReport lang tc =
      runTestCase lang tc
        >>= either (return . Left) (report tc)