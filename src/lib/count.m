/*
**	count:		returns a list of the integers from a to b.
**
**		count a b = [a, a+1, ... b-1; b]
*/
module
export	count;
rec

count a b = if a > b then [] else a . count (a+1) b
end
