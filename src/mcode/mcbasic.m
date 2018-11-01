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
#include "limit.h"
#include "machine.tt"

export mcbasic, mcbasicupdate;
mcbasic svf t g G bv V S = 
	let ts = tmplbl t in
	let bcs = strbv bv in
        let x = valbv bv in
        let (nsvf, vec) = addthemb svf (constrbv bv) in
	let iscon (GvInt  x) = if MININTTAB <= x & x <= MAXINTTAB then
	 			(true, 
	 		      	 if x < 0 then 
			      		"CIm"@itos(-x) 
				 else 
				 	"CIp"@itos x)
			     else
			     	(false, ts)
	 || iscon (GvChar x) = if chr MINCHAR <= x & x <= chr MAXCHAR then
	 			(true, "CC"@itos (ord x))
			     else
			        (false, ts)
	in
	let (con,ns) = iscon bv in
	let rec (c, d, uv, u.us, k) = M nsvf (t+1) g G V S1
--                                           name of constr
	and    (c1, S1) = newS (idlit ns) u S c
	in (	Mcom ("CBASIC "@bcs@" "@itos x).
		(if con then c1 else
		Mdata.
		Mlabel ts.
		Mword (glob bcs).
		Mword (const x).
		hpwords vec @
		Mtext.c1),
			d, uv, us, k)
and
mcbasicupdate svf t g G bv m V S =
	let bcs = strbv bv in
        let x = valbv bv in
	let (move, offs) = 
		case bv in
		   GvSFloat f : (Mmovesf (fconst f), 1+sfloatsize) 
		|| GvDFloat f : (Mmovedf (fconst f), 1+dfloatsize) 
	        || _ : (Mmove (const x), 2) 
	        end in
        let (nsvf, vec) = addthemb svf (constrbv bv) in
	let (c, d, uv, us, k) = M nsvf t g G V S
	and (bco, br) = intoareg (indS (m-1) S) V S
	in (	Mcom ("CBASIC "@bcs) . Mcom("UPDATE "@itos m).
		bco @
		Mmove (idlit bcs) (regind br 0).
		move              (regind br 1).
                hpmoves vec       (regind br offs) @
                c,
			d, uv, us, k)
end
