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
#include "mvectable.t"
#include "handmade.mcode.t"
#include "limit.h"

export mcnil, mcpair, mcstring, mctag, mcvek, mcfloat, mcinteger, mmvector;
mcnil svf ct t g G m V S =
	let ts = tmplbl t in
	let (con, ns) = if m <= MAXTAG0 then (true, "CN"@itos m) else (false, ts) in
        let (nsvf, vec) = addthem svf ct in
	let rec (c, d, uv, u.us, k) = M nsvf (t+1) g G V S1
--                                           name of node
	and    (c1, S1) = newS (idlit ns) u S c
	in (	Mcom ("CNIL "@itos m).
		(if con then c1 else
		Mdata.
		Mlabel ts.
		Mword mniltag.
		Mword (const m).
                hpwords vec @
		Mtext.c1), d, uv, us, k)
and
mcpair svf ct t i G V abS =
	let (a.b.S) = abS in
	let ts = tmplbl t in
        let nsize = hpmsize 3 in
        let (nsvf, vec) = addthem svf ct in
	let rec (c, d, uv, u.us, k) = M nsvf (t+1) 1 G V S1
	    and (c1, S1) = newheapS nsize u S c
	in 
	    (	Mcom("CPAIR").
	    	Mmove i tohp.
	    	Mmove (H 1 a) tohp.
		Mmove (H 2 b) tohp.
                hpmoves vec   tohp @
		c1,
		d, uv, Wuse.Wuse.us, k+nsize)
and
mcstring svf t g G s V S =
        let (nsvf,vec) = addthem svf ("_String", "_List") in
	let ts1 = tmplbl t 
	and ts2 = tmplbl (t+1)
	in
	let rec (c, d, uv, u.us, k) = M nsvf (t+2) g G V S1
	and    (c1, S1) = newS (idlit ts1) u S c
	in (	Mcom ("CSTRING").
		Mdata.
		(if LinkWord then [Mword (const 0)] else [])@
		Mlabel ts1.
		Mword (glob "STRINGN").
		Mword (glob ts2).
		Mword (const (length s)).
		hpwords vec@
		Mlabel ts2.
		Mstring s.
		Mtext.
		c1,
			ts1.d, uv, us, k)
and
mcfloat dbl svf t g G s V S =
        let (nsvf, vec) = addthemb svf (if dbl then "_DFloat" else "_SFloat") in
	let ts1 = tmplbl t 
	in
	let rec (c, d, uv, u.us, k) = M nsvf (t+1) g G V S1
	and    (c1, S1) = newS (idlit ts1) u S c
	in (	Mcom ("CFLOAT").
		Mdata.
		Mlabel ts1.
		Mword (glob (if dbl then "DFLOAT" else "SFLOAT")).
		(if dbl then Mdfloat s else Msfloat s).
                hpwords vec@
		Mtext.
		c1,
			ts1.d, uv, us, k)
and
mcinteger svf t g G s V S =
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
	let rec (c, d, uv, u.us, k) = M nsvf (t+2) g G V S1
	and    (c1, S1) = newS (idlit ts1) u S c
	in (	Mcom ("CINTEGER").
		Mdata.
		(if LinkWord then [Mword (const 0)] else [])@
		Mlabel ts1.
		Mword (glob "BIGNUM").
		Mword (glob ts2).
                hpwords vec@
		(if LinkWord then [Mword (const 0)] else [])@
		Mlabel ts2.
		Mword (glob "DVEK").
		Mword (const (length is)).
		map (Mword o const) is@
		Mtext.
		c1,
			ts1.d, uv, us, k)

and
mctag svf ct t G m V aS =
	let (a.S) = aS in
	let ts = tmplbl t in
        let nsize = hpmsize 3    in
        let (nsvf, vec) = addthem svf ct in
	let rec (c, d, uv, u.us, k) = M nsvf (t+1) 1 G V S1
	and    (c1, S1) = newheapS nsize u S c 
	in (	Mcom ("CTAG "@itos m).
		Mmove mtagtag   tohp.
		Mmove (const m) tohp.
		Mmove (H 2 a)   tohp.
	        hpmoves vec     tohp@
                c1,
		d, uv, Wuse.us, k+nsize )
and
mcvek svf ct t G m tag1 tag2 V S =
	let ts = tmplbl t in
	let hh = m+hpmsize 2 in
        let (nsvf, vec) = addthem svf ct in
	let rec (c, d, uv, u.us, k) = M nsvf (t+1) 1 G V S1
	and     (c1, S1) = newheapS hh u (tail m S) c
	in
	let rec toheap 0 S c = c
	     || toheap n (s.S) c = Mmove s tohp.toheap (n-1) (Ha 1 S) c
	in 
	   (	Mcom("CVEK "@itos m/*@prstk 5 S@prstk 5 S1*/).
		Mmove tag1 tohp.
		Mmove tag2 tohp.
		toheap m (Ha 2 S) (
	        hpmoves vec tohp @
                c1),
		d, uv, prepWuse m us, k+hh)

and mmvector svf i is t g G V S = fail "mmvector"
#if 0
	let (c, d, v, s, k) = M svf t g G V S in
	let iname = mstrid i in
	   (Mcom "CMVECTOR".
	    Mtext.
	    Malign.
	    (if mexported i then [Mexport iname] else [])@
            Mlabel iname.
	    Mword mvektag.
	    Mword (const (length is)).
	    map (\f.Mword (glob (mstrid f))) is@
	    c, [], v, s, k)
#endif

#if 0
where
prstk n S = show_list pamode (head n S)
#endif
end
