module PreludeX where
import Numeric

instance  Read Integer  where
    readsPrec p = readSigned readDec
