module Provider.User (sendMsgIO) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

sendMsgIO :: T.Text -> IO ()
sendMsgIO = TIO.putStrLn
