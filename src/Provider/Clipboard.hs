module Provider.Clipboard (setClipboardIO) where

import Control.Monad (void)
import Data.Functor ((<&>))
import Interface
import Types

setClipboardIO :: (HasExecutor m, HasOs m) => String -> m (Either AppError ())
setClipboardIO str = do
  os <- detectOs
  let cmds = case os of
        Linux -> [Cmd ["xclip", "-selection", "clipboard"]]
        WSL -> [Cmd ["iconv", "-t", "utf16"], Cmd ["clip.exe"]]
        Mac -> [Cmd ["pbcopy"]]
        Windows -> [Cmd ["clip"]]
   in executeCmds cmds (Stdin str) <&> void
