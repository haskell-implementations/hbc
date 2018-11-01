
/*
**	exists:	does a predicate hold for any elements in a list
**
**		exists p [ b1; b2; ... bn ] = p b1 | p b2 | ... | p bn
*/
module
export	exists;
rec
	exists p []	= false
||	exists p (a.b)	= p a | exists p b
end
