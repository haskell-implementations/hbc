module I_Text_Integer() where
import P_T_util
import LMLintegerMisc
-- Standard integral types

instance  Text Integer  where
    readsPrec p = readSigned readDec
#if 0
    showsPrec   = showSigned showInt
#else
    showsPrec p n s = if n < 0 && p > 6 then '(':itos n++(')':s) else itos n ++ s
#endif
    showsType x = showString "Integer"

itos n = {-:"PInteger2String":-} 10 n
