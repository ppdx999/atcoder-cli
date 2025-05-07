module Provider.Utils (try, maybeToExceptT) where

import Control.Monad.Catch (MonadCatch)
import qualified Control.Monad.Catch as Catch
import Control.Monad.Trans.Except (ExceptT, throwE)
import qualified Data.Text as T
import GHC.IO.Exception (IOException)
import Types (AppError (ProviderError))

try :: (MonadCatch m) => m a -> m (Either AppError a)
try ma =
  Catch.try ma
    >>= either
      (\e -> pure $ Left (ProviderError (T.pack $ show (e :: IOException))))
      (pure . Right)

maybeToExceptT :: (Monad m) => e -> Maybe a -> ExceptT e m a
maybeToExceptT err = maybe (throwE err) pure