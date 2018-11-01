module PreludeX where
import LMLintegerMisc

instance  Show Integer  where
    showsPrec p n s = if n < 0 && p > 6 then '(':itos n++(')':s) else itos n ++ s
        where itos n = {-:"PInteger2String":-} 10 n
