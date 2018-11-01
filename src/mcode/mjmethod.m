module
#include "mcodedef_t.t"
#include "../transform/hexpr.t"
#include "../Gcode/Gcodedef_t.t"
#include "../Gcode/Gprint.t"
#include "Wuse.t"
#include "mutil.t"
#include "mutil1.t"
#include "mregs.t"
#include "mstrlbl.t"
#include "mstrid.t"
#include "mprint.t"
#include "mmemcheck.t"
#include "handmade.mcode.t"
#include "machine.tt"
#define VFcode 4
export mjmethod, mcallmethod, mvectordef, mreg2;
rec
    mjmethod svf t G i m aS =  -- i is metno, m is arity
	let (f.a.S) = aS in
	let (c, d, uv, us, k) = M svf t 0 G [] []
	and (c2, r) = intoareg f [] (reg 0.aS)
	in (	Mcom ("JMETHOD "@itos i).
	        c2@
		intoreg0 a (
		Mmove (regind r 1) indreg.
		Mmove (const i) argcreg.
		Mjumptag (onumtag VFcode) (regof indreg).
		c), d, allWpush, Wuse./*Wreg 0*/Wuse.allWpush, 0)
and mcallmethod svf t G i m ns aS = -- i is metno, m is arity -- !!! handle stubs
	let (f.a.S) = aS in
	let ts = tmplbl t in
	let rec (c, d, uv, u.us, k) = M svf (t+1) 0 G allpopV S1
	and (c2, r) = intoareg f [] (reg 0.aS)
	and (c1, S1) = newS (reg 0) u allpopS c
	in (	Mcom ("CALLMETHOD "@itos i@" "@prstk 6 S).
	        c2@
		intoreg0 a (
		Mmove (retaddr ts) pushV.
		Mmove (Srel(m+1)) pushV.
		Mmove (regind r 1) indreg.
		Mmove (const i) argcreg.
		Mjumptag (onumtag VFcode) (regof indreg).
		Mlabel ts.
		retfixup@
		c1),
			d, allWpush, Wuse./*Wreg 0*/Wuse.allWpush, 0)

and mvectordef svf t g (PUSH 0.GET _.CASE n cl Notalabel.G) =
	let (ltab, G') = getlbls G in
	let cl' = map (trlbl ltab) cl in
	let (c, d, uv, us, k) = M svf (t+1) 0 G' [] [] in
	(Mcom "CASE (vector)".case cl' in
	   [l] : Mjump l.c
	|| [l1;l2] : Mcompare argcreg (const 0).Mjcond eq l1.Mjump l2.c
	|| _ : Mcase argcreg 0 (n-1) n cl' t.c
	end, jrefs ltab cl @ d, allWpush, allWpush, 0)
||  mvectordef svf t g ((g1 as CONSTBLOCK _).MOVE _.POP _.(G as JGLOBAL _ i._)) & (isidfail i) =
	-- Methodless class, cannot be called!
	M svf t 0 (g1.G) [] []
||  mvectordef _ _ _ G = fail ("bad vectordef "@Gprint (head 5 G))
-- A real hack!  Recognize the sequences where having tos in r0 works,
-- and push it otherwise.
and mreg2 svf t g G V S & (okG G) = M svf t g G V (reg (-1).reg 0.S)
||  mreg2 svf t g (POP 1.G) V S = 
	let (c, d, uv, us, k) = mmemcheck svf t G V (popS.S)
	in  (Mmove (reg 0) pushS.c, d, uv, us, k)
||  mreg2 _   _ _ G          _ _ = fail ("mreg2: "@Gprint (head 5 G))
and okG (POP 2.JGLOBAL _ _._) = true
||  okG (POP 1.JGLOBAL _ _._) = true
||  okG _ = false
and trlbl ltab (_,_,x) = 
	    case assocdef x ltab None in
	       None : mstrlbl x
	    || Some (m,i) : 'J'.itos m@mstrid i
	    end
and getlbls G =
	if farcase then
	    getlbls' [] [] G
	else
	    ([], G)
and jrefs ltab [] = []
||  jrefs ltab ((_,_,x).xs) =    
	    case assocdef x ltab None in
	       None : jrefs ltab xs
	    || Some (m,i) : mstrid i . jrefs ltab xs
	    end
and getlbls' l rg (LABEL n.ANNOT _.POP 2.JGLOBAL m s.gs) = getlbls' ((n, Some (m,s)).l) rg gs
||  getlbls' l rg ((g as LABEL n).gs) = getlbls' ((n, None).l) (g.rg) gs
||  getlbls' l rg (gs as (FUNEND._)) = (l, reverse rg @ gs)
||  getlbls' l rg (g.gs) = getlbls' l (g.rg) gs
end
