/*
**	index:		Search for the list l1 in the list l2 and return its
**			starting position.  I.e. l1 = tail (index l1 l2) l2.
**			If the l2 doesn't contain l1 as a sublist -1
**			is returned.
**
**		index [x; y; z] [a; b; x; y; z] = 2
*/
module
export	index;
rec index l1 l2 = (
	let len = length l1 in
	f l2 0 where rec
	       f [] n = -1	-- continue to end of list instead of
				-- checking the length to avoid length
				-- on an infinite list
	    || f l n =	if l1 = head len l then n
			else f (tl l) (n+1))
-- stuff below to make it self contained
and
   head 0 l = []
|| head n [] = []
|| head n (a.b) = a.head (n-1) b
and
   length l = len 0 l
where rec
   len n [] = n
|| len n (a.b)	= len (n+1) b
end
