module Usecase.InitSpec (spec) where

import qualified Data.Set as Set
import Mock
import System.FilePath ((</>))
import Test.Hspec
import Types
import Usecase.Init (initContest)

spec :: Spec
spec = describe "Usecase.Init.initContest" $ do
  let contestId = ContestId "abc999"
  let contestDir = "abc999"
  let problemIdA = ProblemId "a"
  let problemIdB = ProblemId "b"
  let problems = [problemIdA, problemIdB]
  let problemDirA = contestDir </> "a"
  let problemDirB = contestDir </> "b"

  it "正常系: コンテストディレクトリと問題ディレクトリを作成し、ログを出力する" $ do
    -- 1. Arrange
    let initialState = initialMockState {msFetchProblemIds = Right problems}
    (result, finalState) <- execMockApp (initContest contestId) initialState

    -- 2. Act
    result `shouldBe` Right ()

    -- 作成されたディレクトリの検証
    msCreatedDirs finalState `shouldBe` Set.fromList [contestDir, problemDirA, problemDirB]

  it "異常系: 問題リストの取得に失敗した場合、エラーを返し処理を中断する" $ do
    -- 1. Arrange
    let fetchError = ProviderError "Network timeout"
    let initialState = initialMockState {msFetchProblemIds = Left fetchError}
    (result, finalState) <- execMockApp (initContest contestId) initialState

    -- 2. Act
    result `shouldBe` Left fetchError

    -- 3. Assert
    msCreatedDirs finalState `shouldBe` Set.fromList [contestDir]