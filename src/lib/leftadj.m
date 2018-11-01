/*
**	leftadj:	pads a list to a given length with a given object.
**
**		leftadj "xxx" 5 'y' = "xxxyy"
*/
module
export	leftadj;
-- WARNING: not self contained
leftadj s n x = s @ rept (n - length s) x
end
