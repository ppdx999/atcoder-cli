module Usecase.Problem
  ( problem,
  )
where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Interface
import Types

problem ::
  ( HasLogger m,
    HasAtcoder m,
    HasConfig m,
    HasProblem m,
    HasUser m
  ) =>
  m (Either AppError ())
problem = runExceptT $ do
  lift $ logInfo "Fetching problem statement..."

  task <- ExceptT loadTask

  cached <- ExceptT loadProblemHtml
  html <- case cached of
    Just h -> do
      lift $ logInfo "Using cached problem HTML"
      return h
    Nothing -> do
      lift $ logInfo "Fetching problem HTML from AtCoder..."
      h <- ExceptT $ fetchProblemHtml task
      _ <- ExceptT $ saveProblemHtml h
      return h

  terminalText <- lift $ htmlToTerminal html
  lift $ sendMsg terminalText
