/*
**	Iitos:		convert an Integer to a string.
*/
module
export	Iitos;
rec
Iitos n = 
#if 1
    Integer2String 10 n
#else
    if n < 0# then
	'-'.itos1 (-# n) []
    else 
	itos1 n []
and itos1 n s = 
    if n < 10# then
	chr (Integer2Int n + ord '0') . s
    else 
	case IntegerDivMod n 10# in
	    (q, r) : itos1 q (chr (Integer2Int r + ord '0') . s)
        end
#endif
end
