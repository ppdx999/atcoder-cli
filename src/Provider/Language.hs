{-# LANGUAGE OverloadedStrings #-}

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
    { langName = LangName "c",
      sourceFile = SourceFile "main.c"
    }

cpp :: Language
cpp =
  Language
    { langName = LangName "cpp",
      sourceFile = SourceFile "main.cpp"
    }

python :: Language
python =
  Language
    { langName = LangName "python",
      sourceFile = SourceFile "main.py"
    }

java :: Language
java =
  Language
    { langName = LangName "java",
      sourceFile = SourceFile "Main.java"
    }

haskell :: Language
haskell =
  Language
    { langName = LangName "haskell",
      sourceFile = SourceFile "Main.hs"
    }

go :: Language
go =
  Language
    { langName = LangName "go",
      sourceFile = SourceFile "main.go"
    }

rust :: Language
rust =
  Language
    { langName = LangName "rust",
      sourceFile = SourceFile "main.rs"
    }

zig :: Language
zig =
  Language
    { langName = LangName "zig",
      sourceFile = SourceFile "main.zig"
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
      let SourceFile src = sourceFile lang
       in if src `elem` files then Just lang else Nothing

    maybeLang :: Maybe Language -> Either AppError Language
    maybeLang = maybe (Left $ ProviderError "No matching language found") Right
