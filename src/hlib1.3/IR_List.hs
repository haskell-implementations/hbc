module PreludeX where
instance  (Read a) => Read [a]  where
    readsPrec p = readList
