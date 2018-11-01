
/*
**	all:	does a predicate hold for all elements in a list
**
**		all p [ b1; b2; ... bn ] = p b1 & p b2 & ... & p bn
*/
module
export	all;
rec
	all p []	= true
||	all p (a.b)	= p a & all p b
end
