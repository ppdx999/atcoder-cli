{-# LANGUAGE OverloadedStrings #-}

module Domain.Usecase.Init
  ( initContest,
  )
where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (ExceptT))
import Data.Foldable (traverse_)
import qualified Data.Text as T
import Domain.Ports
import Domain.Types
import System.FilePath ((</>))

initContest ::
  ( HasFetchProblemList m,
    HasCreateDirectory m,
    HasLogInfo m
  ) =>
  ContestId ->
  ExceptT DomainError m ()
initContest contestId@(ContestId contestName) = do
  lift $ logInfo $ "Initializing contest: " <> contestName

  let contestDir = T.unpack contestName
  lift $ logInfo $ "Creating directory: " <> T.pack contestDir
  ExceptT $ createDirectory contestDir

  lift $ logInfo "Fetching problem list..."
  problemIds <- ExceptT $ fetchProblemList contestId

  lift $ logInfo $ "Found " <> T.pack (show $ length problemIds) <> " problems."

  -- 3. Create problem directories using traverse
  -- traverse はリストの各要素に関数を適用し、結果を収集する (Applicative/Monad の文脈で)
  -- ここでは、各ディレクトリ作成が ExceptT DomainError m () を返すようにする
  let createProblemDir (ProblemId problemName) = do
        let problemDir = contestDir </> T.unpack problemName
        lift $ logInfo $ "Creating directory: " <> T.pack problemDir
        ExceptT $ createDirectory problemDir -- 失敗したら ExceptT が Left を伝播

  -- traverse createProblemDir problemIds は ExceptT DomainError m [()] 型になる
  -- いずれか一つでも createProblemDir が Left を返すと、全体の結果が Left になる
  traverse_ createProblemDir problemIds -- 結果の [()] は不要なので捨てる