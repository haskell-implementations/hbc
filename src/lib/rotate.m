/*
**	rotate:		rotate a list to the left.
**
**		rotate [u;v;x;y;z] 1 = [v;x;y;z;u]
*/
module
export rotate;
rec

   rotate [] n = []
|| rotate l 0 = l
|| rotate (a.b) n = rotate (b @ [a]) (n-1)
end
