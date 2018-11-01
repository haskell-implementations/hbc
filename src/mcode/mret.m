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
export mret, munwind;
mret svf t G S =
	let (a._) = S in
	let (c, d, uv, us, k) = M svf t 0 G [] []
	in (	Mcom("RET").
		intoreg0 a (
		Mmove (Vrel 1) Vp.
		mreturn @ c), d, allWpush, Wuse.allWpush, 0)
and
munwind svf t G aS =
	let (a.S) = aS in
	let (c, d, uv, us, k) = M svf t 0 G [] []
	in (	Mcom "UNWIND".
		intoreg0 a ( 
		Mmove (regind 0 0) indreg.
		(if Trace then [Mcall "do_unwind"] else []) @
		Mjumptag ounwind (regof indreg).c ),
			d, allWpush, Wreg 0.allWpush, 0)
end
