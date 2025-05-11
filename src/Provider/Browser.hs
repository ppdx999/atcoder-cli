module Provider.Browser (openBrowserIO) where

import Data.Functor (void, (<&>))
import qualified Data.Text as T
import Interface
import Text.URI (URI, render)
import Types

openBrowserIO :: (HasExecutor m, HasOs m) => URI -> m (Either AppError ())
openBrowserIO url = do
  os <- detectOs
  let url' = T.unpack $ render url
  let cmd = case os of
        Linux -> Cmd ["xdg-open", url']
        WSL -> Cmd ["wsl-open", url']
        Mac -> Cmd ["open", url']
        Windows -> Cmd ["cmd", "/c", "start", "", url']
   in executeCmd cmd (Stdin "") <&> void