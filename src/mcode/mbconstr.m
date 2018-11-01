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
#include "mvectable.t"
#include "machine.tt"

export mbconstr;
mbconstr svf t G bc vV S =
	let (v.V) = vV in
	let (move, offs) = if bc = Gbsfloat then (Mmovesf, 1+sfloatsize) else if bc = Gbdfloat then (Mmovedf, 1+dfloatsize) else (Mmove, 2) in
        let nsize = hpmsize offs 	in
        let bcs = strbc bc 	in
        let (nsvf, vec) = addthemb svf (constrbc bc) in
	let rec (c, d, uv, u.us, k) = M nsvf t 1 G V S1
	and     (c1, S1) = newheapS nsize u S c
	in (	Mcom ("BCONSTR "@bcs).
		Mmove (idlit bcs) tohp.
		move v tohp.
                hpmoves vec tohp @
                c1,
			d, Wuse.uv, us, k+nsize)
end
