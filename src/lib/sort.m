/*
**	sort:		Sort a list
*/
module

export sort;
rec
    sort lt [] = []
||  sort lt (a.b) = insert a (sort lt b)
	where rec insert a [] = [a]
		||insert a (b.r) = if lt b a then b.insert a r
				   else a.b.r
end
