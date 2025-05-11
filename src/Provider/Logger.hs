module Provider.Logger
  ( logInfoIO,
    logErrorIO,
  )
where

logInfoIO :: String -> IO ()
logInfoIO = putStrLn

logErrorIO :: String -> IO ()
logErrorIO msg = putStrLn $ "[ERROR] " <> msg
