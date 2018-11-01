module PreludeX where
import Numeric

instance  Read Int  where
    readsPrec p = readSigned readDec
