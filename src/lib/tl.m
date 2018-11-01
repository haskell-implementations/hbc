/*
**	tl:		tail of a list.
*/
module
export tl;
    tl []    = fail "tl on []"
||  tl (_.a) = a
end
