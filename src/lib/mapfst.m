/*
**	mapfst:		applies f to each first component in a list.
**
*/
module
export	mapfst;
rec
    mapfst f [] = []
||  mapfst f ((x,y).l) = (f x, y).mapfst f l
end
