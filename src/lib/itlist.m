/*
**	itlist:		iterate a computation over all elements of a list
**			starting from the tail.
**
**		itlist f [x1; x2; ... xm] z = f x1 (f x2 ( ... f xm z)) ... )
*/
module
export	itlist;
rec

   itlist f [] z = z
|| itlist f (a.l) z = f a (itlist f l z)
end
