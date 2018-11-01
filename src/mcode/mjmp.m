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
#include "mmemcheck.t"
export mjmp, mjrel, mjfalse, mjtrue;
mjmp svf t G l =
	let (c, d, uv, us, k) = M svf t 0 G [] []
	in (	Mcom "JMP".
		Mjump (mstrlbl l).c, d, allWpush, allWpush, 0)
and
mjrel svf t G ap p op l abV =
	let (comp, fc) = if ap = Gbsfloat then (Mcomparesf, sfcc) else if ap = Gbdfloat then (Mcomparedf, dfcc) else (Mcompare, (\x.x)) in
	let (a.b.V) = abV in
	let (c, d, uv, us, k) = mmemcheck svf t G allpopV allpopS
	and ls = mstrlbl l
	in (	Mcom (strop op @ " ;JFALSE "@ls).
		comp a b.
		Mjcond ((fc o mopcc o ccneg o ccrev o ccop) op) ls.c,
			d, Wuse.Wuse.allWpush,allWpush,0)
and
mjfalse svf t G aV l =
	let (a.V) = aV in
	let (c, d, uv, us, k) = mmemcheck svf t G allpopV allpopS
	and ls = mstrlbl l
	in (	Mcom ("JFALSE "@ls).
		Mcompare a (const 0).
		Mjcond eq ls.c,
			d, Wuse.allWpush,allWpush,0)
and
mjtrue svf t G aV l =
	let (a.V) = aV in
	let (c, d, uv, us, k) = mmemcheck svf t G allpopV allpopS
	and ls = mstrlbl l
	in (	Mcom ("JTRUE "@ls).
		Mcompare a (const 0).
		Mjcond ne ls.c,
			d, Wuse.allWpush,allWpush,0)
end
