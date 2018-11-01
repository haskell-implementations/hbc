module
/*	CONSTR for user defined types and SPLIT
**
*/
#include "../Gcode/Gcodedef_t.t"
#include "limit.h"

export mconstr, msplit;

rec
    mconstr ct n 0 = [ CNIL ct n ]
 || mconstr ct n 1 = [ CTAG ct n ]
 || mconstr ct n l = let vek 1 = []
		      || vek 2 = [ CPAIR ct (-1) ]
		      || vek l = [ CVEK ct l ]
		     in if n < MAXPAIR then
		   	  vek(l-1) @ [ CPAIR ct n ]
		        else
		          vek l @ [ CTAG ct n ]

and msplit bs n 0 = []
 || msplit bs n l = PUSH 0.reverse (f (mconstr (fail "msplit-ct") n l) bs)
	where rec
	      f (CVEK _ n.G) bs =
		let cbs = head n bs in
		let b = Or cbs in
		if b then
		    SPLITVEK cbs n. f G (true.tail n bs)
		else
		    f G (false.tail n bs)
	||    f (CPAIR _ _.G) (b1.b2.bs) =
		if b1|b2 then
		    SPLITPAIR b1 b2. f G (true.bs)
		else
		    f G (false.bs)
	||    f (CTAG _ _.G) (b.bs) =
		if b then
		    SPLITTAG. f G (b.bs)
		else
		    f G (false.bs)
	||    f [] _ = []
end
