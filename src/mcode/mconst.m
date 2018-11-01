module
#include "../expr/id.t"
#include "../expr/constr_t.t"
#include "../misc/flags.t"
#include "mcodedef_t.t"
#include "../Gcode/Gcodedef_t.t"
#include "../Gcode/Gprint.t"
#include "Wuse.t"
#include "mutil.t"
#include "mutil1.t"
#include "mregs.t"
#include "mstrlbl.t"
#include "mstrid.t"
#include "mvectable.t"
#include "handmade.mcode.t"
#include "limit.h"

export mconstblock;

rec disp (CNIL ct m.gs) ss t svf mss =
	let ts = tmplbl t in
	let (con, ns) = if m <= MAXTAG0 then (true, "CN"@itos m) else (false, ts) in
        let (nsvf,vec) = addthem svf ct in
	disp gs (ns.ss) (t+1) nsvf ((
		Mcom ("CNIL "@itos m).
		(if con then [] else
		Mlabel ts.
		Mword mniltag.
		Mword (const m).
                hpwords vec
		)) . mss)
||  disp (MKAP cs.gs) ss t svf mss =
	pair maptag gs ss t svf (cs,cs) mss
||  disp (MKCAP cs.gs) ss t svf mss =
	pair mcaptag gs ss t svf (cs,cs) mss
||  disp (CPAIR ct m.gs) ss t svf mss =
	pair (mpairtag m) gs ss t svf ct mss
||  disp (CSTRING s.gs) ss t svf mss =
        let (nsvf,vec) = addthem svf ("_String", "_List") in
	let ts1 = tmplbl t 
	and ts2 = tmplbl (t+1) in
	disp gs (ts1.ss) (t+2) nsvf ((
		Mcom ("CSTRING").
		(if LinkWord then [Mword (const 0)] else [])@
		Mlabel ts1.
		Mword (glob "STRINGN").
		Mword (glob ts2).
		Mword (const (length s)).
		hpwords vec@
		Mlabel ts2.
		Mstring s.[]) . mss)
||  disp (CONSTR _ (Gsfloat s) _ _.gs) ss t svf mss =
        let (nsvf, vec) = addthemb svf "_SFloat" in
	let ts = tmplbl t in
	disp gs (ts.ss) (t+1) nsvf ((
		Mcom ("CSFLOAT").
		Mlabel ts.
		Mword (glob "SFLOAT").
		Msfloat s.
                hpwords vec) . mss)
||  disp (CONSTR _ (Gdfloat s) _ _.gs) ss t svf mss =
        let (nsvf, vec) = addthemb svf "_DFloat" in
	let ts = tmplbl t in
	disp gs (ts.ss) (t+1) nsvf ((
		Mcom ("CDFLOAT").
		Mlabel ts.
		Mword (glob "DFLOAT").
		Mdfloat s.
                hpwords vec) . mss)
||  disp (CONSTR _ (Ginteger s) _ _.gs) ss t svf mss =
        let (nsvf, vec) = addthemb svf "_Integer" in
	let ts1 = tmplbl t 
	and ts2 = tmplbl (t+1)
	in
#if 1
	let is = Integer2IntList (stoiI s) in
#else
	-- Bootstrap code for bignums
	let is = 
		let n = stoi s in
		     if n < 0 then [-1; -n]
                else if n > 0 then [ 1;  n]
                else               [0]
	in
#endif
	disp gs (ts1.ss) (t+2) nsvf ((
		Mcom ("CINTEGER").
		(if LinkWord then [Mword (const 0)] else [])@
		Mlabel ts1.
		Mword (glob "BIGNUM").
		Mword (glob ts2).
                hpwords vec@
		(if LinkWord then [Mword (const 0)] else [])@
		Mlabel ts2.
		Mword (glob "DVEK").
		Mword (const (length is)).
		map (Mword o const) is) . mss)

||  disp (CTAG ct m.gs) (is.ss) t svf mss =
	let ts = tmplbl t in
        let (nsvf, vec) = addthem svf ct in
	disp gs (ts.ss) (t+1) nsvf ((
		Mcom ("CTAG "@itos m).
		(if LinkWord then [Mword (const 0)] else [])@
		Mlabel ts.
		Mword mtagtag.
		Mword (const m).
		Mword (glob is).
		hpwords vec) . mss)
||  disp (CVEK ct m.gs) ss t svf mss = vek gs ss t m ct svf mss mvektag (const m)
||  disp (MKAPLV i m.gs) ss t svf mss = 
	let is = mstrid i in
        let cstr = idtopstr i in
	vek gs ss t m (cstr,cstr) svf mss mvaptag (idlit ('V'.is))
||  disp (CBASIC bv.gs) ss t svf mss =
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
	disp gs (ns.ss) (t+1) nsvf ((
		Mcom ("CBASIC "@bcs@" "@itos x).
		(if con then [] else
		Mlabel ts.
		Mword (glob bcs).
		Mword (const x).
		hpwords vec
		)) . mss)
||  disp (PUSHGLOBAL i.gs) ss t svf mss = disp gs (mstrid i.ss) t svf ([Mcom ("PUSHGLOBAL "@prid i)].mss)
||  disp (g._) _ _ _ _ = fail ("No match in disp: "@Gprint [g])
||  disp [] ss t svf mss = (ss, t, svf, conc mss)

and pair i gs (ias.ibs.ss) t svf ct mss =
	let ts = tmplbl t in
        let (nsvf,vec) = addthem svf ct in
	disp gs (ts.ss) (t+1) nsvf ((
		Mcom("CPAIR").
		(if LinkWord then [Mword (const 0)] else [])@
		Mlabel ts.
		Mword (if i=maptag then mapgtag else i).
		Mword (glob ias).
		Mword (glob ibs).
		hpwords vec) . mss)

and vek gs ss t m ct svf mss tag1 tag2 =
	let ts = tmplbl t in
        let (nsvf, vec) = addthem svf ct in
	disp gs (ts.tail m ss) (t+1) nsvf ((
		Mcom("CVEK "@itos m/*@prstk 5 S@prstk 5 S1*/).
		(if LinkWord then [Mword (const 0)] else [])@
		Mlabel ts.
		Mword (if tag1 = mvaptag then mvapgtag else tag1).
		Mword tag2.
		(if m = 0 then
		    [Mword (const 0)]
		else
		    map (Mword o glob) (head m ss)) @
	        hpwords vec) . mss)

and mconstblock svf t g gs G V S =
	let (nss, t', nsvf, c0) = disp gs [] t svf [] in
	let rec (c, d, uv, us, k) = M nsvf t' g G V S1
	and     m = length nss
	and     f c S []     _      = (c, S)
	     || f c S (n.ns) (u.us) =
		    let rec (c1, S1) = newS (idlit n) u S c2
	    	    and     (c2, S2) = f c S1 ns us
		    in (c1, S2)
	and     (c1, S1) = f c S (reverse nss) (reverse (head m us))
        and     c2 = if all comment c0 then c0 else Mdata.c0@[Mtext]
	in (c2@c1, (if mayupd gs then nss@d else d), uv, tail m us, k)

and comment (Mcom _) = true
||  comment _ = false
and mayupd gs = true --exists mayupd1 gs
and mayupd1 (MKAP _) = true
||  mayupd1 (MKAPLV _ _) = true
||  mayupd1 (PUSHGLOBAL _) = true
||  mayupd1 (CSTRING _) = true
||  mayupd1 _ = false
end
