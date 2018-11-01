/*
**	allsame:	check if all elements of a list are equal.
**
*/
module
export	allsame;
rec
    allsame [] = true
 || allsame (a.b) = alla a b
and alla a [] = true
||  alla a (x.xs) = a=x & alla a xs
end
