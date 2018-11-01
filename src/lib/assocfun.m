/*
**	assocfun:	get value related to i in a (relation, value) list.
**
**		assocfun k [ (p1, v1); ... (pn, vn) ] = vi,
**			where i is the smallest number such that   pi k.
**			Fails if no match.
*/
module
export	assocfun;
rec

   assocfun i [] = fail "assocfun FAILED"
|| assocfun i ((p,v).r) =
	if p i then
		v
	else
		assocfun i r
end
