/*
**	union:		compute union of two sets (lists).
**
**		union l1 l2 = [all x such that: x IN l1 OR x IN l2]
*/
module
export union;
rec union l1 l2 = l1 @ difference l2 l1
-- self contained
and difference [] _ = []
||  difference (x.xs) ys =
    if amem x ys then
	difference xs ys
    else
	x.difference xs ys
and amem a []	  = false
||  amem a (x.xs) = x=a | amem a xs
end
