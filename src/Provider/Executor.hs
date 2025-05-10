module Provider.Executor (executeCmdIO) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TEnc
import Provider.Utils (try)
import System.Process (readProcess)
import Types

executeCmdIO :: Cmd -> Stdin -> IO (Either AppError Stdout)
executeCmdIO (Cmd cmd) (Stdin stdin) =
  let bsToStr = T.unpack . TEnc.decodeUtf8
      strToBs = TEnc.encodeUtf8 . T.pack
   in try (readProcess (head cmd) (tail cmd) (bsToStr stdin))
        >>= either (return . Left) (return . Right . Stdout . strToBs)
