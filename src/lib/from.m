/*
**	from:		infinite list of integers starting from n.
**
**		from n = n . (n+1) . (n+2) .  ...
*/
module
export from;
rec

from n = n . from (n+1)
end
