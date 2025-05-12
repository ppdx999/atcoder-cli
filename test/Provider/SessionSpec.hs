module Provider.SessionSpec (spec) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Mock (MockState (..), execMockApp, initialMockState)
import Provider.Session (loadSessionIO, saveSessionIO)
import Test.Hspec (Spec, describe, it, shouldBe)
import Types (AppError (InvalidSession, ProviderError, SessionNotFound), Session (Session))

spec :: Spec
spec = describe "Provider.Sesseion" $ do
  describe "loadSession" $ do
    it "正常系" $ do
      -- 1. Arrange
      let initState =
            initialMockState
              { -- Mock HasFileSystem
                msDoesFileExist = [True],
                msReadFile = Right "valid session",
                -- Mock HasConfig
                msLoadSessionPath = Right "/mock/session.txt"
              }
      -- 2. Act
      (result, _finalState) <- execMockApp loadSessionIO initState
      -- 3. Assert
      result `shouldBe` Right (Session "valid session")

    it "Sessionファイルが存在しない場合は、Left SessionNotFoundを返す" $ do
      -- 1. Arrange
      let initState =
            initialMockState
              { -- Mock HasFileSystem
                msDoesFileExist = [False],
                -- Mock HasConfig
                msLoadSessionPath = Right "/mock/session.txt"
              }
      -- 2. Act
      (result, _finalState) <- execMockApp loadSessionIO initState
      -- 3. Assert
      result `shouldBe` Left SessionNotFound

    it "Sessionの値が不正な場合は Left InvalidSessionを返す" $ do
      -- 1. Arrange
      let initState =
            initialMockState
              { -- Mock HasFileSystem
                msDoesFileExist = [True],
                msReadFile = Right "", -- invalid session value
                -- Mock HasConfig
                msLoadSessionPath = Right "/mock/session.txt"
              }
      -- 2. Act
      (result, _finalState) <- execMockApp loadSessionIO initState
      -- 3. Assert
      result `shouldBe` Left (InvalidSession "Session cannot be empty.")

    it "Sessionファイルの読み込みに失敗した場合、Left ProviderErrorを返す" $ do
      -- 1. Arrange
      let initState =
            initialMockState
              { -- Mock HasFileSystem
                msDoesFileExist = [True],
                -- Mock HasConfig
                msReadFile = Left (ProviderError "Read File Error"),
                msLoadSessionPath = Right "/mock/session.txt"
              }
      -- 2. Act
      (result, _finalState) <- execMockApp loadSessionIO initState
      -- 3. Assert
      result `shouldBe` Left (ProviderError "Read File Error")

  describe "saveSession" $ do
    it "正常系" $ do
      -- 1. Arrange
      let initState =
            initialMockState
              { -- Mock HasConfig
                msLoadSessionPath = Right "/mock/session.txt"
              }
      -- 2. Act
      (result, finalState) <- execMockApp (saveSessionIO (Session "a session")) initState
      -- 3. Assert
      result `shouldBe` Right ()

      msCreatedDirs finalState `shouldBe` Set.singleton "/mock"
      msSaveFile finalState `shouldBe` Map.singleton "/mock/session.txt" "a session"

    it "Sessionファイルの親ディレクトリが存在しない場合、ディレクトリを作成してからSessionファイルを作る" $ do
      -- 1. Arrange
      let initState =
            initialMockState
              { -- Mock HasConfig
                msLoadSessionPath = Right "/unexist/dirs/session.txt"
              }
      -- 2. Act
      (result, finalState) <- execMockApp (saveSessionIO (Session "a session")) initState
      -- 3. Assert
      result `shouldBe` Right ()

      msCreatedDirs finalState `shouldBe` Set.singleton "/unexist/dirs"
      msSaveFile finalState `shouldBe` Map.singleton "/unexist/dirs/session.txt" "a session"