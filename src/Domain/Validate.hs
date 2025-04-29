{-# LANGUAGE OverloadedStrings #-}

module Domain.Validate
  ( mkContestId,
    mkProblemId,
    mkLanguageId,
    mkSourceFile,
  )
where

import qualified Data.Text as T
import Domain.Types

mkContestId :: T.Text -> Either DomainError ContestId
mkContestId t
  | T.null t = Left (InvalidContestId "Contest ID cannot be empty.")
  | T.all validChar t = Right (ContestId t)
  | otherwise = Left (InvalidContestId ("Invalid characters in Contest ID: " <> t))
  where
    validChars :: T.Text
    validChars = "abcdefghijklmnopqrstuvwxyz0123456789-"
    validChar :: Char -> Bool
    validChar c = T.any (== c) validChars

mkProblemId :: T.Text -> Either DomainError ProblemId
mkProblemId t
  | T.null t = Left (InvalidProblemId "Problem ID cannot be empty.")
  | T.all validChar t = Right (ProblemId t)
  | otherwise = Left (InvalidProblemId ("Invalid characters in Problem ID: " <> t))
  where
    validChars :: T.Text
    validChars = "abcdefghijklmnopqrstuvwxyz"
    validChar :: Char -> Bool
    validChar c = T.any (== c) validChars

mkLanguageId :: Int -> Either DomainError LanguageId
mkLanguageId n
  | n > 0 = Right (LanguageId n)
  | otherwise = Left (InvalidLanguageId n)

mkSourceFile :: FilePath -> Either DomainError SourceFile
mkSourceFile path
  | null path = Left (SourceFileNotFound path)
  | otherwise = Right (SourceFile path)
