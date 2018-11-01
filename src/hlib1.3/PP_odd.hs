module PreludeX where
odd :: (Integral a) => a -> Bool
odd n = not (n `rem` 2 == 0)
