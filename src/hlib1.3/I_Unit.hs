module PreludeX where
data () = () deriving (Eq, Ord, Enum, Ix)
instance Bounded () where
    maxBound = ()
    minBound = ()
