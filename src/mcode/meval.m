module
#include "mcodedef_t.t"
#include "../Gcode/Gcodedef_t.t"
#include "../misc/flags.t"
#include "Wuse.t"
#include "mutil.t"
#include "mutil1.t"
#include "mregs.t"
#include "mstrlbl.t"
#include "mstrid.t"
#include "mprint.t"
#include "handmade.mcode.t"
#include "mmemcheck.t"
#include "machine.tt"
#define OTEVAL (36/4)
export meval, mevalupdunw, mteval;
rec
meval svf t g G ns V aS =	-- !!! handle ns
	let (a.S) = aS in
	let rec (c, d, uv, u.us, k) = if needcheck G then mmemcheck svf (t+1) G allpopV S1 else M svf (t+1) g G allpopV S1
	and     (c1, S1) = newS (reg 0) u allpopS c
	in 
	if TestEval then
	    let lab = tmplbl t in
	    let laba = lab @ "a" in
	    (	Mcom "EVAL".
		intoreg0 a (
		Mmove (regind 0 0) indreg.
		Mcompare indreg (idlit "CANON").     -- Unevaluated before evaluated (better name?)
		Mjcond lt laba.                  
		Mtext1.
		Mlabel laba.
		(if null retfixup then
			[Mmove (retaddr lab) (pushV);
			 Mjumptag oeval (regof indreg)]
		 else
			[Mcalltag oeval (regof indreg);
			 Mjump lab]) @
		Mtext.
		Mlabel lab.
		c1), d, allWpush, Wreg 0.allWpush, 0)
	else
	    (	Mcom "EVAL".
		intoreg0 a (
		Mmove (regind 0 0) indreg.
		Mcalltag oeval (regof indreg).
		c1),
			d, allWpush, Wreg 0.allWpush, 0)
and
mevalupdunw svf t g G V aS =
	let (a.S) = aS in
	let rec (c, d, uv, us, k) = M svf t g G [] []
	in (	Mcom "EVAL; UPDATE 1; UNWIND".
		intoreg0 a (
                (if Trace then [Mcall "do_evalupdunw"] else []) @
		Mjumpf "evalupdunw".c), 
			d, allWpush, Wreg 0.allWpush, 0)
and
mteval svf t g G V aS =
	let (a.S) = aS in
	let (cr, r) = intoareg a V S in
	let rec (c, d, u.uv, us, k) = M svf t g G V1 S
	and    (c1, V1) = newV (regind r OTEVAL) u V c
	in ( Mcom"TEVAL". cr @ [ Mmove (regind r 0) (reg r)] @ c1, d, uv, Wuse.us, k)

and
   needcheck [] = false
|| needcheck (EVAL _._) = false
|| needcheck (LABEL _._) = false
|| needcheck (JMP _._) = false
|| needcheck (JFALSE _._) = false
|| needcheck (JFUN _._) = false
|| needcheck (CALLFUN _ _._) = false
|| needcheck (UNWIND._) = false
|| needcheck (ALLOC _._) = true
|| needcheck (BCONSTR _._) = true
|| needcheck (MKAP _._) = true
|| needcheck (MKCAP _._) = true
|| needcheck (MKAPLV _ _._) = true
|| needcheck (CONSTR _ _ _ _._) = true
|| needcheck (CASE _ _ _._) = false
|| needcheck (RET._) = false
|| needcheck (JGLOBAL _ _._) = false
|| needcheck (CALLGLOBAL _ _ _._) = false
|| needcheck (BUPDRET _ _._) = false
|| needcheck (CUPDRET _ _ _ _._) = false
|| needcheck (UPDRET _._) = false
|| needcheck (JMETHOD _ _._) = false
|| needcheck (CALLMETHOD _ _ _._) = false
|| needcheck (CNIL _ _._) = true
|| needcheck (CPAIR _ _._) = true
|| needcheck (CTAG _ _._) = true
|| needcheck (CVEK _ _._) = true
|| needcheck (CBASIC _._) = true
|| needcheck (CSTRING _._) = true
--|| needcheck (CMVECTOR _ _._) = true
|| needcheck (_.gs) = needcheck gs

end
