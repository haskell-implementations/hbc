/*
**	sublist:	starting with number n, extract m elements from l.
**
**		sublist 2 3 [a;b;c;d;e;f] = [b;c;d]
*/
module export sublist;
rec

   sublist n m [] = []
|| sublist n 0 l = []
|| sublist 1 m (x.l) = x . sublist 1 (m-1) l
|| sublist n m (x.l) = sublist (n-1) m l
end
