/*
**	anysame:	check if any elements of a list are equal.
**
*/
module
export	anysame;
rec
    anysame [] = false
 || anysame (a.b) = amem a b | anysame b
and amem a []	  = false
||  amem a (x.xs) = x=a | amem a xs
end
