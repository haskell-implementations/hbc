/*
**	rightadj:	pads a list to a given length with a given object.
**
**		rightadj "42" 5 ' ' = "   42"
*/
module
-- WARNING: not self contained
export	rightadj;

rightadj s n x = rept (n - length s) x @ s
end
