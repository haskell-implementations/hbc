/*
**	Maxl:		find largest element in a list
**				fails on empty list
**
**		Maxl [x1; x2; ... xn] = xi where xi >= xj, 1 <= j <= n
*/
module
export Maxl;

   Maxl [] = fail "Maxl on []"
|| Maxl (x.xs) = max x xs
	where rec max m [] = m
	||        max m (x.xs) = if m>x then max m xs else max x xs
end
