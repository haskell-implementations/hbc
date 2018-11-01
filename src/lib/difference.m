/*
**	difference:	compute difference between two sets (lists).
**
**		difference l1 l2 = [all x such that: x IN l1 AND NOT x IN l2]
*/
module
export difference;
--difference l1 l2 = filter (\x.~ (mem x l2)) l1
rec difference [] _ = []
||  difference (x.xs) ys =
    if amem x ys then
	difference xs ys
    else
	x.difference xs ys
and amem a []	  = false
||  amem a (x.xs) = x=a | amem a xs
end
