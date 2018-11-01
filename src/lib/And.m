/*
**	And:		and between all elements in a list.
**
**		And [ b1; b2; ... bn ] = b1 & b2 & ... & bn
*/
module
export	And;
rec
	And []		= true
||	And (a.b)	= a & And b
end
