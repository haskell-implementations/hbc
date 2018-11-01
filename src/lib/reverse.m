/*
**	reverse:	reverse a list.
*/
module
export	reverse;

reverse l = (rev1 [] l
		  where rec rev1 rl [] = rl
			|| rev1 rl (a.b) = rev1 (a.rl) b)
end
