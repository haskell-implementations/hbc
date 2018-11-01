module PreludeX where
instance Show Bool where
    showsType _ = showString "Bool"
    showsPrec _ True = showString "True"
    showsPrec _ False = showString "False"
