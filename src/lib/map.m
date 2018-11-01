/*
**	map:		applies f to each element in a list.
**
**		map f l
*/
module
export	map;
rec

   map f [] = []
|| map f (a.b) = f a . map f b
end
