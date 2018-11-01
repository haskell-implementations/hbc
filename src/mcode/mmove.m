module
#include "mcodedef_t.t"
#include "../Gcode/Gcodedef_t.t"
#include "Wuse.t"
#include "mutil.t"
#include "mutil1.t"
#include "mregs.t"
#include "mstrlbl.t"
#include "mstrid.t"
#include "handmade.mcode.t"
#include "mprint.t"
export mmove;
mmove svf t g G m V aS =
	let (a.S) = aS in
	let rec subst m r (Sind n.S2)&(n=m) = r . subst m r S2
             || subst m r (popS.S2) =  popS.S2
	     || subst m r (a.S2) = a . subst m r S2
        in
	let rec has m (Sind n.S2)&(n=m) = true
	     || has m (popS.S2) = false
	     || has m (a.S2) = has m S2
	in		 
	let rec f a m (popS.S1) = ([ Mmove a (Sind m) ], popS.S1)
	     || f a 0 (x.S1)    = ([], a.S1)
	     || f a m (x.S1)    = let (c2,S2) = f a (m-1) S1 in (c2,x.S2)
	     || f _ _ _         = fail "mmove,f"
	   and fu m (Wpush.us) = Wpush.us
	    || fu 0 (u.us) = Wuse.us -- u could be Wreg
	    || fu m (u.us) = u.fu (m-1) us
	in
	let r = reg(gareg aS V) in
	let rec (c, d, uv, us, k) = M svf t g G V SS
	and     (cm, Sm) = f a (m-1) S
	and     (cs, SS) = case cm in 
		              [Mmove _ (Sind m)] & (has m Sm): (Mmove (Sind m) r.cm, subst m r Sm)
		     	   || _ : (cm, Sm)
			   end
	in
	    (Mcom("MOVE "@itos m@" "@prstk 6 aS@" "@prWuses 4 us). cs @ c, d, uv, Wuse.fu (m-1) us, k)
end
