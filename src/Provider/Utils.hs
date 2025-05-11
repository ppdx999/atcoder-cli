module Provider.Utils
  ( try,
    maybeToExceptT,
    decodeUtf8,
    encodeUtf8,
    mkURI,
    dashToUnderscore,
    crlfToLf,
    takeWhileEnd,
  )
where

import Control.Monad.Catch (MonadCatch, MonadThrow)
import qualified Control.Monad.Catch as Catch
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TEnc
import GHC.IO.Exception (IOException)
import qualified Text.URI as URI
import Types (AppError (ProviderError))

try :: (MonadCatch m) => m a -> m (Either AppError a)
try ma =
  Catch.try ma
    >>= either
      (\e -> pure $ Left (ProviderError (show (e :: IOException))))
      (pure . Right)

maybeToExceptT :: (Monad m) => e -> Maybe a -> ExceptT e m a
maybeToExceptT err = maybe (throwE err) pure

decodeUtf8 :: ByteString -> String
decodeUtf8 = T.unpack . TEnc.decodeUtf8

encodeUtf8 :: String -> ByteString
encodeUtf8 = TEnc.encodeUtf8 . T.pack

mkURI :: (MonadThrow m) => String -> m URI.URI
mkURI = URI.mkURI . T.pack

dashToUnderscore :: String -> String
dashToUnderscore ('-' : xs) = '_' : dashToUnderscore xs
dashToUnderscore (x : xs) = x : dashToUnderscore xs
dashToUnderscore [] = []

crlfToLf :: String -> String
crlfToLf ('\r' : '\n' : xs) = '\n' : crlfToLf xs
crlfToLf (x : xs) = x : crlfToLf xs
crlfToLf [] = []

takeWhileEnd :: (Char -> Bool) -> String -> String
takeWhileEnd f =
  reverse . takeWhile f . reverse