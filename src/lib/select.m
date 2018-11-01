/*
**	select:		pick n:th element from a list.
**
**		select 1 l = hd l
*/
module
export	select;
rec

   select n [] = fail ("select "@itos n)
|| select 1 (a.b) = a
|| select n (a.b) = select (n-1) b
end
