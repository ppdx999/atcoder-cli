{-# LANGUAGE OverloadedStrings #-}

module Provider.TestCase (loadTestCasesIO, saveTestCaseIO, reportTestResultIO) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TEnc
import Interface
import System.FilePath (takeBaseName, takeExtension, (</>))
import Types (AppError, RunTestCaseResult (RunTestCaseResult), TestCase (TestCase))
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

reportTestResultIO :: (HasUser m) => (TestCase, RunTestCaseResult) -> m (Either AppError ())
reportTestResultIO (TestCase name input want, RunTestCaseResult got) = do
  if got == want
    then do
      sendMsg $ "TestCase - " <> name <> " : OK"
      pure $ Right ()
    else do
      sendMsg $ "TestCase - " <> name <> " : Fail"
      sendMsg "Input:"
      sendMsg $ TEnc.decodeUtf8 input
      sendMsg ""
      sendMsg "Want:"
      sendMsg $ TEnc.decodeUtf8 want
      sendMsg ""
      sendMsg "Got:"
      sendMsg $ TEnc.decodeUtf8 got
      sendMsg ""
      pure $ Right ()
