/*
**	group:		group a list of items;
**			given a list of items it returns a list of a list of
**			items, where the elements in the sublist are "equal"
**			(according to the predicate 'eq') and are consecutive
**			items in the list 'l'.
**
**		group eq l
*/
module
export group;
rec
    group eq l = choplist f l
	         where f (a.b) = take (eq a) (a.b)
-- stuff below to make it selfcontained
and choplist f []	= []
||  choplist f l	= let (a,b) = f l in a . choplist f b
and take f l = (take1 [] l
		  where rec take1 l1 []    = (rev1 [] l1, [])
			 || take1 l1 (a.b) = if f a then take1 (a.l1) b
					    else (rev1 [] l1, a.b)
		  and       rev1 rl [] = rl
			 || rev1 rl (a.b) = rev1 (a.rl) b)
end
