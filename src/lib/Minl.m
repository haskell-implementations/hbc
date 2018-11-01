/*
**	Minl:		find smallest element in a list,
**				fails on empty list
**
**		Minl [x1; x2; ... xn] = xi where xi <= xj, 1 <= j <= n
*/
module
export Minl;

   Minl [] = fail "Minl on []"
|| Minl (x.xs) = min x xs
	where rec min m [] = m
	||        min m (x.xs) = if m<x then min m xs else min x xs
end
