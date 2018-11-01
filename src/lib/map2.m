/*
**	map2:		applies f to pair of elements from two lists.
**
*/
module
export	map2;
rec
    map2 f (x1.l1) (x2.l2) = f x1 x2 . map2 f l1 l2
||  map2 f _ _ = []
end
