module
/* Small printf, takes a list of integers an produces TEXT. */
#include "text_t.t"
export pri;

    pri l I =
	let rec p [] _ = []
	     || p (c.l) (i.I) = 
		  if c = '^' then
			Ti i . p l I
		  else
			Ts [c]  . p l (i.I)
	     || p l [] = [ Ts l ]
	in
		Tl (p l I)		
end
