module PreludeX where
succ :: (Enum a) => a -> a
succ n = toEnum (fromEnum n + 1)
