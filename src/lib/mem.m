/*
**	mem:		test if x is = to any element of a list.
**
**		mem x [a; b; c; ... ; z ] =     x = a | x = b | ... | x = z
*/
module
export	mem;
rec

   mem x []	= false
|| mem x (a.b)	= x=a | mem x b
end
