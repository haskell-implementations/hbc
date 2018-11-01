module PreludeX where
pred :: (Enum a) => a -> a
pred n = toEnum (fromEnum n - 1)
