module Provider.TestCase (loadTestCasesIO, saveTestCaseIO, reportTestResultIO) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
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

        pure $ TestCase name input output
    )
    names

saveTestCaseIO :: (MonadIO m, HasFileSystem m, HasLogger m, HasConfig m) => TestCase -> m (Either AppError ())
saveTestCaseIO (TestCase tcName tcInput tcOutput) = runExceptT $ do
  lift $ logInfo "save testcase ..."
  testDir <- ExceptT loadTestDir

  ExceptT $ createDirectoryIfMissing True testDir

  let inFile = testDir </> tcName <> ".in"
  let outFile = testDir </> tcName <> ".out"

  _ <- ExceptT $ saveFile inFile tcInput

  ExceptT $ saveFile outFile tcOutput

reportTestResultIO :: (HasUser m, HasLogger m) => (TestCase, RunTestCaseResult) -> m (Either AppError ())
reportTestResultIO (TestCase name input want, RunTestCaseResult got) = do
  logInfo "report test result..."
  if got == want
    then do
      sendMsg $ "TestCase - " <> name <> " : OK"
      pure $ Right ()
    else do
      sendMsg $ "TestCase - " <> name <> " : Fail"
      sendMsg "Input:"
      sendMsg $ visualizeWhitespace input
      sendMsg ""
      sendMsg "Want:"
      sendMsg $ visualizeWhitespace want
      sendMsg ""
      sendMsg "Got:"
      sendMsg $ visualizeWhitespace got
      sendMsg ""
      pure $ Right ()
  where
    visualizeWhitespace =
      concatMap replaceChar
      where
        replaceChar c = case c of
          ' ' -> "[SP]"
          '\t' -> "[TAB]"
          '\n' -> "[LF]\n"
          _ -> [c]
