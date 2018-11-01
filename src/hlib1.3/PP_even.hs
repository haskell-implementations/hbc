module PreludeX where
even :: (Integral a) => a -> Bool
even n = n `rem` 2 == 0
