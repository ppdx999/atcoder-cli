module Provider.Stdin (readLineIO) where

import Data.Text (Text, pack)

readLineIO :: IO Text
readLineIO = fmap pack getLine