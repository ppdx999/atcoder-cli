module Provider.Problem
  ( loadProblemCachePathIO,
    loadProblemHtmlIO,
    saveProblemHtmlIO,
    htmlToTerminalIO,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.Char (isSpace)
import Data.Functor ((<&>))
import Data.List (dropWhileEnd, isInfixOf)
import qualified Interface as I
import Text.HTML.TagSoup
import Types

loadProblemCachePathIO :: (MonadIO m, I.HasFileSystem m, I.HasLogger m) => m (Either AppError FilePath)
loadProblemCachePathIO = do
  I.logInfo "load problem cache path..."
  currentDir <- I.getCurrentDirectory
  return $ Right (currentDir <> "/problem.html")

loadProblemHtmlIO ::
  (MonadIO m, I.HasFileSystem m, I.HasLogger m) =>
  m (Either AppError (Maybe String))
loadProblemHtmlIO = do
  I.logInfo "load problem html from cache..."
  cachePath <- loadProblemCachePathIO
  case cachePath of
    Left err -> return $ Left err
    Right path -> do
      exists <- I.doesFileExist path
      if exists
        then I.readFile path <&> fmap Just
        else return $ Right Nothing

saveProblemHtmlIO ::
  (MonadIO m, I.HasFileSystem m, I.HasLogger m) =>
  String ->
  m (Either AppError ())
saveProblemHtmlIO html = do
  I.logInfo "save problem html to cache..."
  cachePath <- loadProblemCachePathIO
  case cachePath of
    Left err -> return $ Left err
    Right path -> I.saveFile path html

htmlToTerminalIO :: (MonadIO m, I.HasLogger m) => String -> m String
htmlToTerminalIO html = do
  I.logInfo "converting html to terminal format..."
  return $ convertHtmlToTerminal html

convertHtmlToTerminal :: String -> String
convertHtmlToTerminal html =
  let tags = parseTags html
      taskStatement = extractTaskStatement tags
   in formatTags taskStatement

extractTaskStatement :: [Tag String] -> [Tag String]
extractTaskStatement tags = go tags 0 []
  where
    go [] _ acc = reverse acc
    go (TagOpen "div" attrs : rest) 0 acc
      | isTaskStatement attrs =
          go rest 1 acc
    go (TagOpen "div" _ : rest) depth acc
      | depth > 0 = go rest (depth + 1) (TagOpen "div" [] : acc)
    go (TagClose "div" : rest) depth acc
      | depth > 1 = go rest (depth - 1) (TagClose "div" : acc)
      | depth == 1 = reverse acc
    go (t : rest) depth acc
      | depth > 0 = go rest depth (t : acc)
      | otherwise = go rest depth acc

    isTaskStatement attrs =
      case lookup "id" attrs of
        Just idVal | "task-statement" `isInfixOf` idVal -> True
        _ -> case lookup "class" attrs of
          Just cls | "task-statement" `isInfixOf` cls -> True
          _ -> False

formatTags :: [Tag String] -> String
formatTags tags = unlines $ filter (not . all isSpace) $ lines $ go tags 0 ""
  where
    go :: [Tag String] -> Int -> String -> String
    go [] _ acc = acc
    go (TagOpen "h2" _ : TagText t : TagClose "h2" : rest) _ acc =
      go rest 0 (acc <> "\n" <> bold ("## " <> strip t) <> "\n")
    go (TagOpen "h3" _ : TagText t : TagClose "h3" : rest) _ acc =
      go rest 0 (acc <> "\n" <> bold ("### " <> strip t) <> "\n")
    go (TagOpen "h4" _ : TagText t : TagClose "h4" : rest) _ acc =
      go rest 0 (acc <> "\n" <> "#### " <> strip t <> "\n")
    go (TagOpen "p" _ : rest) _ acc =
      go rest 0 acc
    go (TagClose "p" : rest) _ acc =
      go rest 0 (acc <> "\n")
    go (TagOpen "pre" _ : TagText t : TagClose "pre" : rest) _ acc =
      go rest 0 (acc <> "\n" <> codeBlock t <> "\n")
    go (TagOpen "pre" _ : rest) _ acc =
      let (content, rest') = collectPreContent rest
       in go rest' 0 (acc <> "\n" <> codeBlock content <> "\n")
    go (TagOpen "var" _ : TagText t : TagClose "var" : rest) _ acc =
      go rest 0 (acc <> cyan t)
    go (TagOpen "li" _ : rest) n acc =
      go rest n (acc <> "  - ")
    go (TagClose "li" : rest) n acc =
      go rest n (acc <> "\n")
    go (TagOpen "ul" _ : rest) n acc =
      go rest (n + 1) acc
    go (TagClose "ul" : rest) n acc =
      go rest (n - 1) acc
    go (TagOpen "br" _ : rest) n acc =
      go rest n (acc <> "\n")
    go (TagText t : rest) n acc =
      go rest n (acc <> t)
    go (_ : rest) n acc = go rest n acc

    collectPreContent :: [Tag String] -> (String, [Tag String])
    collectPreContent = collect ""
      where
        collect acc (TagClose "pre" : rest) = (acc, rest)
        collect acc (TagText t : rest) = collect (acc <> t) rest
        collect acc (TagOpen "var" _ : TagText t : TagClose "var" : rest) = collect (acc <> t) rest
        collect acc (_ : rest) = collect acc rest
        collect acc [] = (acc, [])

strip :: String -> String
strip = dropWhileEnd isSpace . dropWhile isSpace

bold :: String -> String
bold s = "\ESC[1m" <> s <> "\ESC[0m"

cyan :: String -> String
cyan s = "\ESC[36m" <> s <> "\ESC[0m"

codeBlock :: String -> String
codeBlock s =
  let border = "────────────────────"
   in dim border <> "\n" <> s <> dim border
  where
    dim t = "\ESC[2m" <> t <> "\ESC[0m"
