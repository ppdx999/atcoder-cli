module Provider.Clipboard (setClipboardIO) where

import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.Functor ((<&>))
import Interface
import Types

setClipboardIO :: (HasExecutor m, HasOs m) => ByteString -> m (Either AppError ())
setClipboardIO bytes = do
  os <- detectOs
  let cmd = case os of
        Linux -> Cmd ["xclip", "-selection", "clipboard"]
        WSL -> Cmd ["clip.exe"]
        Mac -> Cmd ["pbcopy"]
        Windows -> Cmd ["clip"]
   in executeCmd cmd (Stdin bytes) <&> void