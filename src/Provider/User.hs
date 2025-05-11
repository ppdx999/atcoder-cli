module Provider.User (sendMsgIO) where

sendMsgIO :: String -> IO ()
sendMsgIO = putStrLn
