/*
**	array:	array handling
**	
*/
module 
export arrmake, arrindex, arrupdate;
rec

type array (*a)  = noinfo + node (int # (*a) # array (*a) # array (*a) )
and
arrmake i j x =
	if i > j then
		noinfo
	else let m = (i+j)/2 in
		node(m, x, arrmake i (m-1) x, arrmake (m+1) j x)
and
arrindex (node(i,x,a1,a2)) j = 
	if j = i then
		x
	else
	if j < i then
		arrindex a1 j
	else
		arrindex a2 j
and
arrupdate (node(i,x,a1,a2)) j y =
	if j = i then
		node(i, y, a1, a2)
	else
	if j < i then
		node(i, x, arrupdate a1 j y, a2)
	else
		node(i, x, a1, arrupdate a2 j y)
end

