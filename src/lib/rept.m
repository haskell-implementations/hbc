/*
**	rept:		constructs a list of n duplicates of x.
**
*/
module
export rept;
rec

rept n x =
	if n < 1 then [] else x . rept (n-1) x
end
