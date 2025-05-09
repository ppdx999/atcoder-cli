module Usecase.Test (test) where

import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import Data.Foldable (traverse_)
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
  lang <- ExceptT detectLanguage
  ExceptT $ buildLanguage lang
  tcs <- ExceptT loadTestCases
  traverse_ (ExceptT . runAndReport lang) tcs
  ExceptT $ cleanupBuildFile lang
  where
    runAndReport :: (HasLanguage m, HasReporter m) => Language -> TestCase -> m (Either AppError ())
    runAndReport lang tc =
      runTestCase lang tc
        >>= either (return . Left) (report tc)