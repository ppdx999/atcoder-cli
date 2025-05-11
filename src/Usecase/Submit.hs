{-# LANGUAGE OverloadedStrings #-}

module Usecase.Submit (submit) where

import Control.Monad.Trans (MonadTrans (lift))
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import Interface
import Types
import Prelude hiding (readFile)

submit :: (HasLogger m, HasConfig m, HasLanguage m, HasFileSystem m, HasClipboard m, HasBrowser m, HasAtcoder m) => m (Either AppError ())
submit = runExceptT $ do
  lift $ logInfo "Submit ..."
  lang <- ExceptT detectLanguage
  src <- ExceptT $ readFile (sourceFile lang)
  ExceptT $ setClipboard src

  task <- ExceptT loadTask
  url <- lift $ submitPageUrl task
  ExceptT $ openBrowser url