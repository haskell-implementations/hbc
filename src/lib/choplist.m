/*
**	choplist:	chops up a list into a new list; each chop is made with
**			f which should return a pair, the chopped part and the
**			rest of the list.
**
**		choplist func list
*/
module
export	choplist;
rec

   choplist f []	= []
|| choplist f l	= let (a,b) = f l in a . choplist f b
end
