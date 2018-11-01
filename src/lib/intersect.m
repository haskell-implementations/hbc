/*
**	intersect:	compute intersection of two sets (lists).
**
**		intersect l1 l2 = [all x such that: x IN l1 AND x IN l2]
*/
module
export intersect;
--intersect l1 l2 = filter (\x. mem x l2) l1
rec intersect [] _ = []
||  intersect (x.xs) ys =
    if amem x ys then
	x.intersect xs ys
    else
	intersect xs ys
and amem a []	  = false
||  amem a (x.xs) = x=a | amem a xs
end
