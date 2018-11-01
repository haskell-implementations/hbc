/*
**	member:		tests if equality predicate eq holds for x and
**			any element in a list.
**
**		member (\x.\y.x=y) 3 [2;3;4;5]	= true
*/
module

export	member;
rec

--	member is essentially the same as:   Or (map (eq x) l)

   member eq x [] = false
|| member eq x (h.t) = eq x h | member eq x t

end
