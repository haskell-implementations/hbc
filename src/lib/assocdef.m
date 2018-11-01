/*
**	assocdef:	same as assoc but with a default value supplied.
**
**		assoc k [(j1, v1); ... (jn, vn)] def =
**			vi,	where i is the smallest number such that k = ji,
**			def,	if no such number exists.
*/
module
export	assocdef;
rec

   assocdef i [] d = d
|| assocdef i ((k,v).r) d =
	if i = k then
		v
	else
		assocdef i r d
end
