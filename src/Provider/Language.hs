module Provider.Language
  ( detectLanguageIO,
    c,
    cpp,
    python,
    java,
    haskell,
    go,
    rust,
    zig,
    langs,
    buildLanguageIO,
    runTestCaseIO,
    cleanupBuiltFileIO,
  )
where

import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import Data.Functor ((<&>))
import Data.Maybe (catMaybes, listToMaybe)
import Interface
import System.FilePath (takeFileName)
import Types

c :: Language
c =
  Language
    { langName = "c",
      sourceFile = "main.c",
      buildCmd = Just (Cmd ["gcc", "-o", "main", "main.c"]),
      builtFile = Just "main",
      runCmd = Cmd ["./main"]
    }

cpp :: Language
cpp =
  Language
    { langName = "cpp",
      sourceFile = "main.cpp",
      buildCmd = Just (Cmd ["g++", "-o", "main", "main.cpp"]),
      builtFile = Just "main",
      runCmd = Cmd ["./main"]
    }

python :: Language
python =
  Language
    { langName = "python",
      sourceFile = "main.py",
      buildCmd = Nothing,
      builtFile = Nothing,
      runCmd = Cmd ["python", "main.py"]
    }

java :: Language
java =
  Language
    { langName = "java",
      sourceFile = "Main.java",
      buildCmd = Just (Cmd ["javac", "Main.java"]),
      builtFile = Just "Main",
      runCmd = Cmd ["java", "Main"]
    }

haskell :: Language
haskell =
  Language
    { langName = "haskell",
      sourceFile = "Main.hs",
      buildCmd = Just (Cmd ["ghc", "--make", "-no-keep-hi-files", "-no-keep-o-files", "Main.hs"]),
      builtFile = Just "Main",
      runCmd = Cmd ["./Main"]
    }

go :: Language
go =
  Language
    { langName = "go",
      sourceFile = "main.go",
      buildCmd = Just (Cmd ["go", "build", "-o", "main", "main.go"]),
      builtFile = Just "main",
      runCmd = Cmd ["./main"]
    }

rust :: Language
rust =
  Language
    { langName = "rust",
      sourceFile = "main.rs",
      buildCmd = Just (Cmd ["rustc", "-o", "main", "main.rs"]),
      builtFile = Just "main",
      runCmd = Cmd ["./main"]
    }

zig :: Language
zig =
  Language
    { langName = "zig",
      sourceFile = "main.zig",
      buildCmd = Just (Cmd ["zig", "-c", "main.zig"]),
      builtFile = Just "main",
      runCmd = Cmd ["./main"]
    }

langs :: [Language]
langs = [c, cpp, python, java, haskell, go, rust, zig]

detectLanguageIO :: (HasFileSystem m) => m (Either AppError Language)
detectLanguageIO = runExceptT $ do
  cd <- ExceptT $ fmap Right getCurrentDirectory
  files <- ExceptT $ readDir cd <&> fmap (map takeFileName)

  ExceptT $ return $ maybeLang $ takeFirst $ map (findLangInFiles files) langs
  where
    takeFirst :: [Maybe Language] -> Maybe Language
    takeFirst = listToMaybe . catMaybes

    findLangInFiles :: [FilePath] -> Language -> Maybe Language
    findLangInFiles files lang =
      let src = sourceFile lang
       in if src `elem` files then Just lang else Nothing

    maybeLang :: Maybe Language -> Either AppError Language
    maybeLang = maybe (Left $ ProviderError "No matching language found") Right

buildLanguageIO :: (HasExecutor m, HasLogger m) => Language -> m (Either AppError ())
buildLanguageIO lang = do
  logInfo "Building language ..."
  case buildCmd lang of
    Nothing -> return $ Right ()
    Just cmd ->
      executeCmd cmd (Stdin "")
        >>= either (return . Left) showMsg
  where
    showMsg :: (HasLogger m) => Stdout -> m (Either AppError ())
    showMsg (Stdout output) = do
      logInfo $ "Building Language output: " <> output
      return $ Right ()

runTestCaseIO :: (HasExecutor m, HasLogger m) => Language -> TestCase -> m (Either AppError RunTestCaseResult)
runTestCaseIO lang tc = do
  logInfo "Running testcase ..."
  executeCmd (runCmd lang) (Stdin (tcInput tc))
    >>= either (return . Left) (return . Right . toTestCaseResult)
  where
    toTestCaseResult :: Stdout -> RunTestCaseResult
    toTestCaseResult (Stdout bytes) = RunTestCaseResult bytes

cleanupBuiltFileIO :: (HasFileSystem m, HasLogger m) => Language -> m (Either AppError ())
cleanupBuiltFileIO lang = do
  logInfo "Cleaning up build files ..."

  case builtFile lang of
    Nothing -> return $ Right ()
    Just f -> do removeFile f