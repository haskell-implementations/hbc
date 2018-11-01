module
#include "../expr/id.t"
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
#include "../funnos.h"
export mjglobal, mcallglobal, mscallglobal;
mjglobal svf t G i m S =
	let is = mstrid i in
	let is' = itos m @ is in
	let (c, d, uv, us, k) = M svf t 0 G [] [] in
	let c1 = tostack m S
	in (	Mcom ("JGLOBAL "@is').
                (if Trace then [Mcall "do_jglobal"] else []) @
		c1@Mjumpf ('J'.is').c, is.d, allWpush, allWpush, 0)
and
mcallglobal svf tt G i m ns S = -- !!! handle stubs
	let is = mstrid i in
	let is' = itos m @ is in
	let rec (c, d, uv, u.us, k) = M svf (tt+1) 0 G allpopV S1
	and (c1, S1) = newS (reg 0) u allpopS c
	and c2 = tostack m S 
	in 
	    (	Mcom ("CALLGLOBAL "@is').
		Mcall ('S'.is').
		c1,
			is.d, allWpush, allWpush, 0)
and
mscallglobal svf tt G i m =
	let is = mstrid i in
	let is' = is in --itos m @ is in
	let rec (c, d, uv, u.us, k) = M svf tt 0 G allpopV S1
	and (c1, S1) = newS (reg 0) u allpopS c
	in (	Mcom ("SCALLGLOBAL "@is').
		Mcall ('Y'.itos m@is').
		c1,
			is.d, allWpush, allWpush, 0)
end
