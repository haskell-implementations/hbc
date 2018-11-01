/*
**	assoc:		find a value associated to a key in a (key, value) list.
**
**		assoc k [(j1, v1); ... (jn, vn)] = vi,
**			where i is the smallest number such that k = ji.
**			Fails if no match.
*/
module
export	assoc;
rec

   assoc i [] = fail "assoc failed"
|| assoc i ((k,v).r) =
	if i = k then
		v
	else
		assoc i r
end
