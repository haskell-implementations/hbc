/*
**	mapsnd:		applies f to each second component in a list.
**
*/
module
export	mapsnd;
rec
    mapsnd f [] = []
||  mapsnd f ((x,y).l) = (x, f y).mapsnd f l
end
