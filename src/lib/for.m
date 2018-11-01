/*
**	for:		map f over the list of integers from a to b.
**
**		for a b f = [ f a; f(a+1); ... f(b-1); f b]
*/
module
export	for;
rec

for a b f =
	if a > b then
		[]
	else
		f a . for (a+1) b f
end
