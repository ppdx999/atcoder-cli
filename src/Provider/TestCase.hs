{-# LANGUAGE OverloadedStrings #-}

module Provider.TestCase (loadTestCasesIO, saveTestCaseIO) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import qualified Data.Text as T
import Interface
import System.FilePath (takeBaseName, takeExtension, (</>))
import Types (AppError, TestCase (TestCase))
import Prelude hiding (readFile)

loadTestCasesIO :: (MonadIO m, HasConfig m, HasFileSystem m, HasLogger m) => m (Either AppError [TestCase])
loadTestCasesIO = runExceptT $ do
  lift $ logInfo "load test cases ..."
  testDir <- ExceptT loadTestDir
  fnames <- ExceptT (readDir testDir)
  let names = map takeBaseName $ filter (\f -> takeExtension f == ".in") fnames

  traverse
    ( \name -> do
        let inFile = testDir </> name <> ".in"
        let outFile = testDir </> name <> ".out"

        input <- ExceptT $ readFile inFile
        output <- ExceptT $ readFile outFile

        pure $ TestCase (T.pack name) input output
    )
    names

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
