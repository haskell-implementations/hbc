/*
**	hd:		hd of a list.
*/
module
export hd;
    hd []    = fail "hd on []"
||  hd (a._) = a
end
