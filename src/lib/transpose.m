/*
**	transpose:	transpose a matrix (like zip in SASL).
*/
module
-- WARNING: not self contained
export	transpose;
rec

transpose l =
	if Or (map null l) then
		[]
	else
		map hd l . transpose (map tl l)
end
