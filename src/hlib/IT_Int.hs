module I_Text_Int() where
import P_T_util
instance  Text Int  where
    readsPrec p = readSigned readDec
--    showsPrec   = showSigned showInt
    showsType x = showString "Int"

    showsPrec p n s = if n < 0 && p > 6 then '(':itos n++(')':s) else itos n ++ s
itos :: Int -> String
itos n = 
    if n < 0 then
	if -n < 0 then
	    -- n is minInt, a difficult number
	    itos (n `quot` 10) ++ itos' (-(n `rem` 10)) []
	else
	    '-':itos' (-n) []
    else 
	itos' n []

itos' :: Int -> String -> String
itos' n cs = 
    if n < 10 then
	chr (n + ord '0') : cs
    else 
	itos' (n `quot` 10) (chr (n `rem` 10+ord '0') : cs)

