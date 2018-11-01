module PreludeX where
import Numeric

instance  Read Float  where
    readsPrec p = readSigned readFloat

instance  Read Double  where
    readsPrec p = readSigned readFloat

