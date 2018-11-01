/*
**	last:		last element of a list
**
*/
module
export last;
rec

    last [] = fail "last"
||  last [a] = a
||  last (a.b) = last b
end
