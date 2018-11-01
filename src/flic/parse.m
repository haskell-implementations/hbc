-- General parsing functions
-- Phil Wadler
-- 28 Oct 86:  Created.
-- 29 Oct 86:  Modified for continuation style (nxt).
-- 30 Oct 86:  Cleaned up for use with pattern compiler.
--  2 Nov 86:  Changed to use fail.

module
#include "location.t"
export  reply, nxt, nxtok, alt, ok, ko, lit, litp, notlit, loc, parse;
rec
   (type  reply *a *b  =  Ok *a *b + Ko)
and
   nxt p f x	=  case p x in
		      Ko:      Ko
		   || Ok u y:  f u y
		   end
and
   nxtok p f x	=  (f u y
		    where
		    (u,y) = case p x in
		               Ko:      fail (errmsg x)
		            || Ok u y:  (u,y)
		            end)
and
   alt p q x  	=  case p x in
		      Ko:      q x
		   || Ok u y:  Ok u y
		   end
and
   ok u x	=  Ok u x
and
   ko 		=  Ko
and
   lit a ((lc,x).lcxs)	    =  if a = x then Ok a lcxs
					else Ko
and
   litp p ((lc,x).lcxs)	    =  if p x then Ok x lcxs
				      else Ko
and
   notlit a		    =  litp (\b. a ~= b)
and
   loc ((lc,x).lcxs)	    =  Ok lc ((lc,x).lcxs)
and
   errmsg ((lc,x).lcxs)	    =  "\n" @ show_loc lc @ ": Syntax error\n"
and
   parse p x		    =  case p x in
			          Ko:	   fail (errmsg x)
			       || Ok u y:  u
			       end
end
