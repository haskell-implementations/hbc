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
#include "../misc/flags.t"
#include "machine.tt"

export mbconstrupdate, mcpairupdate, mctagupdate, mcnilupdate;

mbconstrupdate svf t g G bc m aV S =
        let (nsvf, vec) = addthemb svf (constrbc bc) in
	let (a.V) = aV in -- must be lazy
	let bcs = strbc bc in
	let (move, offs) = if bc = Gbsfloat then (Mmovesf, 1+sfloatsize) else if bc = Gbdfloat then (Mmovedf, 1+dfloatsize) else (Mmove, 2) in
	let (c, d, uv, us, k) = M nsvf t g G V S
	and (bco, br) = intoareg (indS (m-1) S) aV S
	in (	Mcom ("BCONSTR "@bcs) . Mcom ("UPDATE "@itos m).
		bco @ 
		Mmove (idlit bcs) (regind br 0).
		move a            (regind br 1).
	        hpmoves vec       (regind br offs) @
		c,
			d, Wuse.uv, us, k)
and

mcpairupdate svf ct t g G is n V abS =
	let (nsvf, vec) = addthem svf ct in
	let (a.b.S) = abS in -- must be lazy
	let (c, d, uv, us, k) = M nsvf t g G V S
	and (bco, br) = intoareg (indS (n-1+2) abS) V abS
	in (	Mcom ("CPAIR"). Mcom("UPDATE "@itos n).
		bco @
		Mmove is (regind br 0).
		Mmove a  (regind br 1).
		Mmove b  (regind br 2).
	        hpmoves vec (regind br 3) @
		c,
			d, uv, Wuse.Wuse.us, k)
and

mcnilupdate svf ct t g G m n V S =
	let (nsvf, vec) = addthem svf ct in
	let (c, d, uv, us, k) = M nsvf t g G V S
	and (bco, br) = intoareg (indS (n-1) S) V S
	in (	Mcom ("CNIL "@itos m). Mcom("UPDATE "@itos n).
		bco @
		Mmove mniltag   (regind br 0).
		Mmove (const m) (regind br 1).
	        hpmoves vec     (regind br 2) @
		c,
			d, uv, us, k)

and
mctagupdate svf ct t g G m n V aS =
	let (nsvf, vec) = addthem svf ct in
	let (a.S) = aS in
	let (c, d, uv, us, k) = M nsvf t g G V S
	and (bco, br) = intoareg (indS (n-1+1) aS) V aS
	in (	Mcom ("CTAG "@itos m). Mcom("UPDATE "@itos n).
		bco @
		Mmove mtagtag   (regind br 0).
		Mmove (const m) (regind br 1).
		Mmove a         (regind br 2).
	        hpmoves vec     (regind br 3) @
		c,
			d, uv, Wuse.us, k)
end
