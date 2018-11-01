/*
**	Or:		or between all elements in a list:
**
**		Or [ b1; b2; ... bn ] = b1 | b2 | ... | bn 
*/
module
export	Or;
rec
	Or []		= false
||	Or (a.b)	= a | Or b
end
