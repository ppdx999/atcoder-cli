module Provider.Stdin (readLineIO) where

import Data.Text (Text)

readLineIO :: IO Text
readLineIO = readLn