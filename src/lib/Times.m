/*
**	Times:		multiply all elements in a list.
**
**		Times [ x1; x2; ... xn ] = x1 * x2 * ... * xn
*/
module
export Times;
rec
   Times l = times l 1
and
   times [] a    = a
|| times (x.l) a = times l (x*a)
end
