/*
**	Sum:		sum all elements in alist.
**
**		Sum [ x1; x2; ... xn ] = x1 + x2 + ... + xn
*/
module
export Sum;
rec
   Sum l = sum l 0
and
   sum [] a    = a
|| sum (x.l) a = sum l (x+a)
end
