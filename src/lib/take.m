/*
**	take:		given a predicate and a list return a pair:
**			the first member is the initial segment of the list
**			for which the predicate holds for each element,
**			the second member is the rest of the list,
**			beginning with the first element for which the
**			predicate doesn't hold.
**
**		take (\x.x<5) [1;4;2;6;2;5] = ([1;4;2],[6;2;5])
*/
module
export	take;
rec

take f l = (take1 [] l
		  where rec take1 l1 []    = (rev1 [] l1, [])
			 || take1 l1 (a.b) = if f a then take1 (a.l1) b
					     else (rev1 [] l1, a.b)
			and rev1 rl [] = rl
			||  rev1 rl (a.b) = rev1 (a.rl) b)
end
