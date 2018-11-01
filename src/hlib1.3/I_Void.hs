module PreludeX(Void) where
data Void = __Void
{-
instance Eq Void
instance Ord Void
instance Show Void where
    showsType _ = showString "Void"
-}
