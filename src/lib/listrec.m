/*
**	listrec:	primitive recursion over a list.
*/
module
export listrec;
rec

   listrec z f [] = z
|| listrec z f (x.l) = f x l (listrec z f l)
end
