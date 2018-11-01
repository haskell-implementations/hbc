/*
**	itosf:		convert an integer to a string of given minimal length.
**
**		n <= length (itosf n i)
*/
module
-- WARNING: not self contained
export	itosf;
itosf f n = rightadj (itos n) f ' '
end
