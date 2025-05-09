module TestUtils (isProviderError) where

import Types (AppError (ProviderError))

isProviderError :: Either AppError a -> Bool
isProviderError (Left (ProviderError _)) = True
isProviderError _ = False