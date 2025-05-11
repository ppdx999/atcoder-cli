module Usecase.Download
  ( download,
  )
where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Data.Foldable (traverse_)
import Interface
import Types

download ::
  ( HasLogger m,
    HasAtcoder m,
    HasConfig m,
    HasTestCase m
  ) =>
  m (Either AppError ())
download = runExceptT $ do
  lift $ logInfo "Starting download..."

  ExceptT loadTask
    >>= ExceptT . fetchTestCases
    >>= traverse_ (ExceptT . saveTestCase)