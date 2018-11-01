/*
**	tail:		get all elements beyond the n:th one from a list.
**
**		tail 3 [2;6;4;7;1] = [7;1]
*/
module
export	tail;
rec
    tail 0 l = l
||  tail n [] = []
||  tail n (x.l) = tail (n-1) l
end
