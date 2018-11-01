module
#include "../misc/flags.t"
#include "mcodedef_t.t"
#include "../Gcode/Gcodedef_t.t"
#include "Wuse.t"
#include "mutil.t"
#include "mutil1.t"
#include "mregs.t"
#include "mstrlbl.t"
#include "mstrid.t"
#include "handmade.mcode.t"
#include "machine.tt"
export mjfun, mcallfun;
mjfun svf t G m aS =
	let (a.S) = aS in
	let (c, d, uv, us, k) = M svf t 0 G [] []
	in (	Mcom ("JFUN "@itos m).
		intoreg0 a (
                (if Trace then [Mcall "do_jfun"] else []) @
		Mmove (const m) argcreg.
		Mmove (regind 0 0) indreg.
		Mjumptag ojfun (regof indreg).c), 
			d, allWpush, Wreg 0.allWpush, 0)
and
mcallfun svf t G m ns V aS = -- !!! handle stubs
	let (a.S) = aS in
	let rec (c, d, uv, u.us, k) = M svf (t+1) 0 G allpopV S1 
	and    (c1, S1) = newS (reg 0) u allpopS c
	in (	Mcom ("CALLFUN "@itos m).
		intoreg0 a (
		Mmove (retaddr (tmplbl t)) pushV.
		Mmove (Srel(m+1)) pushV.
		Mmove (const m) argcreg.
		Mmove (regind 0 0) indreg.
		Mjumptag ojfun (regof indreg).
		Mlabel (tmplbl t).
		retfixup@
		c1),
			d, allWpush, Wreg 0.allWpush, 0)
end
