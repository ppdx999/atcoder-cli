module Provider.Os (detectOsIO) where

import Data.List (isInfixOf)
import System.Directory (doesFileExist)
import System.Info (os)
import Types (OS (..))

detectOsIO :: IO OS
detectOsIO = case os of
  "darwin" -> return Mac
  "mingw32" -> return Windows
  "linux" -> do
    isWsl <- isWSL
    return $ if isWsl then WSL else Linux
  _ -> error $ "Unsupported OS: " ++ os
  where
    isWSL :: IO Bool
    isWSL = do
      exists <- doesFileExist "/proc/version"
      if not exists
        then return False
        else do
          content <- readFile "/proc/version"
          return ("Microsoft" `isInfixOf` content || "WSL" `isInfixOf` content)