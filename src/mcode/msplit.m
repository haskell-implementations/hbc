module
#include "mcodedef_t.t"
#include "../Gcode/Gcodedef_t.t"
#include "Wuse.t"
#include "mutil.t"
#include "mutil1.t"
#include "mregs.t"
#include "mstrlbl.t"
#include "mstrid.t"
#include "mprint.t"
#include "handmade.mcode.t"
export msplitpair, msplittag, msplitvek;
msplitpair svf t g b1 b2 G V aS =
	let (a.S) = aS in
	let (cr, r) = intoareg a V S in
	let rec (c, d, uv, u1.u2.us, k) = M svf t g G V S1
	and    (c2, S2) = if b2 then newS (regind r 2) (if b1 then u2 else u1) S c1 else (c1, S)
	and    (c1, S1) = if b1 then newS (regind r 1) u1 S2 c else (c, S2)
	in ( Mcom ("SPLITPAIR "@prstk 6 aS@prWuses 4 (u1.u2.us)).cr @ c2, d, uv, (if b1&b2 then Wuse.us else Wuse.u2.us), k)
and
msplittag svf t g G V aS =
	let (a.S) = aS in
	let (cr, r) = intoareg a V S in
	let rec (c, d, uv, u.us, k) = M svf t g G V S1
	and    (c1, S1) = newS (regind r 2) u S c
	in ( Mcom "SPLITTAG".cr @ c1, d, uv, Wuse.us, k)
and
msplitvek svf t g bs G m' V aS =
	let m = length (filter (\x.x) bs) in
	let (a.S) = aS in
	let (cr, r) = intoareg a V S in
	let rec f 1 c S us     bs = (c, S)
	     || f n c S (u.us) (true.bs) =
		    let rec (c1, S1) = newS (regind r n) u S c2
	    	    and     (c2, S2) = f (n-1) c S1 us bs
		    in (c1, S2)
	     || f n c S us     (false.bs) =
		    f (n-1) c S us bs
	in let rec (c, d, uv, us, k) = M svf t g G V S1
	   and     (c1, S1) = f (m'+1) c S (reverse(head m us)) (reverse bs)
	   in (Mcom ("SPLITVEK "@itos m'). cr @ c1, d, uv, Wuse.(tail m us), k)
end
