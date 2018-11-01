module PreludeX where
import Numeric

instance  Show Float  where
    showsPrec   = showSigned showFloat
    showsType x = showString "Float"

instance  Show Double  where
    showsPrec   = showSigned showFloat
    showsType x = showString "Double"
