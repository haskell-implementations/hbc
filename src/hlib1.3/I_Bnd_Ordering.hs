module PreludeX where
instance Bounded Ordering where
    minBound = LT
    maxBound = GT
