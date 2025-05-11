module Provider.Stdin (readLineIO) where

readLineIO :: IO String
readLineIO = getLine