/*
**	space:		returns a list of n spaces.
*/
module
export	space;

rec space n =
	if n < 1 then "" else ' ' . space (n-1)
end
