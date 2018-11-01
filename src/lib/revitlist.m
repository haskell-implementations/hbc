/*
**	revitlist:	iterate a computation over all elements in a list
**			starting from the head.
**
**		revitlist f [x1;x2; ... xm] z = f xm ( ... (f x2 (f x1 z)) ... )
*/
module
export	revitlist;
rec

   revitlist f [] z = z
|| revitlist f (a.l) z = revitlist f l (f a z)
end
