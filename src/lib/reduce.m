/*
**	reduce:		iterate a computation over all elements of a list
**			starting from the tail.
**
**		reduce f z [x1; x2; ... xm] = f x1 (f x2 ( ... f xm z)) ... )
*/
module
export	reduce;
rec

   reduce f z [] = z
|| reduce f z (a.l) = f a (reduce f z l)
end
