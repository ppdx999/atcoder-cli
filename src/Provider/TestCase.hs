{-# LANGUAGE OverloadedStrings #-}

module Provider.TestCase (saveTestCaseIO) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import qualified Data.Text as T
import Interface (HasConfig (loadTestDir), HasFileSystem (..), HasLogger (logInfo))
import System.FilePath ((</>))
import Types (AppError, TestCase (TestCase))

saveTestCaseIO :: (MonadIO m, HasFileSystem m, HasLogger m, HasConfig m) => TestCase -> m (Either AppError ())
saveTestCaseIO (TestCase tcName tcInput tcOutput) = runExceptT $ do
  lift $ logInfo "save testcase ..."
  testDir <- ExceptT loadTestDir

  ExceptT $ createDirectoryIfMissing True testDir

  let inFile = testDir </> T.unpack tcName <> ".in"
  let outFile = testDir </> T.unpack tcName <> ".out"

  lift $ logInfo $ "Saving " <> T.pack inFile
  _ <- ExceptT $ saveFile inFile tcInput

  lift $ logInfo $ "Saving " <> T.pack outFile
  ExceptT $ saveFile outFile tcOutput
