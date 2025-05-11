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
      let initState =
            initialMockState
              { msDoesFileExistQueue = [True],
                msReadFiles = Right "valid session"
              }
      (result, _finalState) <- execMockApp loadSessionIO initState
      result `shouldBe` Right (Session "valid session")

    it "Sessionファイルが存在しない場合は、Left SessionNotFoundを返す" $ do
      let initState = initialMockState {msDoesFileExistQueue = [False]}
      (result, _finalState) <- execMockApp loadSessionIO initState
      result `shouldBe` Left SessionNotFound

    it "Sessionの値が不正な場合は Left InvalidSessionを返す" $ do
      let initState =
            initialMockState
              { msDoesFileExistQueue = [True],
                msReadFiles = Right "" -- invalid session value
              }
      (result, _finalState) <- execMockApp loadSessionIO initState
      result `shouldBe` Left (InvalidSession "Session cannot be empty.")

    it "Sessionファイルの読み込みに失敗した場合、Left ProviderErrorを返す" $ do
      let initState =
            initialMockState
              { msDoesFileExistQueue = [True],
                msReadFiles = Left (ProviderError "Read File Error")
              }
      (result, _finalState) <- execMockApp loadSessionIO initState
      result `shouldBe` Left (ProviderError "Read File Error")

  describe "saveSession" $ do
    it "正常系" $ do
      let initState =
            initialMockState
              { msCreateDirIfMissing = \_ _ -> Right (),
                msSessionPath = Right "/mock/session.txt"
              }
      (result, finalState) <- execMockApp (saveSessionIO (Session "a session")) initState
      result `shouldBe` Right ()

      msCreatedDirs finalState `shouldBe` Set.singleton "/mock"
      msSavedFiles finalState `shouldBe` Map.singleton "/mock/session.txt" "a session"

    it "Sessionファイルの親ディレクトリが存在しない場合、ディレクトリを作成してからSessionファイルを作る" $ do
      let initState =
            initialMockState
              { msCreateDirIfMissing = \_ _ -> Right (),
                msSessionPath = Right "/unexist/dirs/session.txt"
              }
      (result, finalState) <- execMockApp (saveSessionIO (Session "a session")) initState
      result `shouldBe` Right ()

      msCreatedDirs finalState `shouldBe` Set.singleton "/unexist/dirs"
      msSavedFiles finalState `shouldBe` Map.singleton "/unexist/dirs/session.txt" "a session"