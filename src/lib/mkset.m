/*
**	mkset:	remove duplicate items from a list.
**
*/
module
export	mkset;
rec
    mkset' l []	   = []
||  mkset' l (a.b) = if amem a l then mkset' l b else a.mkset' (a.l) b
and mkset l = mkset' [] l
and amem a []	  = false
||  amem a (x.xs) = x=a | amem a xs
end
