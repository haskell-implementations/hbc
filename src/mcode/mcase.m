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
#include "mtype.t"
#include "mget.t"
#include "machine.tt"
#include "../misc/flags.t"
#include "../runtime/tagtable.h"
export mcase, mgettagcase;
rec butlast = reverse o tl o reverse
and thd (_,_,x) = x
and mcase svf t G max cl ldef V =
	let (a._) = V in
	let (c, d, uv, us, k) = M svf (t+1) 0 G [] []
--                                    temp label in Mcase
	and intoreg (reg r) c = (reg r, c)
	 || intoreg   a     c = (tagreg, Mmove a tagreg.c)
	and rec fill d l h ((n,_,j).r) & (l=n) = j.fill d (l+1) h r
	     || fill d l h r & (l<=h) = d.fill d (l+1) h r
	     || fill _ _ _ _ = []
	in

	(Mcom ("CASE ...").(
	let cnt = length cl
	and (low,_,_) = hd cl
	and (high,_,_) = last cl in
#if 1
	if usecase max low high cnt & high-low > 0 /* overflow check */ then
	    Mcase a low high max (map mstrlbl (fill ldef low high cl)) t.
	    (if ldef = Notalabel then c else Mjump (mstrlbl ldef).c)
	else
	    let rec c1 = itlist (\(n,_,l).\p. 
				  Mcompare r2 (const n). 
				  Mjcond eq (mstrlbl l). p)
			(if max=cnt then butlast cl else cl)
			(Mjump(mstrlbl (if max=cnt then thd (last cl) else ldef)).c)
	    and (r2, c2) = intoreg a c1 
	    in c2
	), d, Wuse.allWpush, allWpush, 0)
#else
	if usecase max low high cnt & high-low > 0 /* overflow check */ then
	    Mcase a low high max (map mstrlbl (fill ldef low high cl)) t.
	    (if ldef = Notalabel then c else Mjump (mstrlbl ldef).c)
	else
	    let (cls, ld) = if max=cnt then (butlast cl, thd (last cl)) else (cl, ldef) in
	    let rec c1 = makecmp t r2 cls (Mjump (mstrlbl ld).c)
	    and (r2, c2) = intoreg a c1 
	    in c2
	), d, Wuse.allWpush, allWpush, 0)
and makecmp t r2 cls c =
    let l = length cls in
    if l < 5 then		-- 5 is an arbitrary limit
	reduce (\(n,_,l).\p. 
		Mcompare r2 (const n). 
		Mjcond eq (mstrlbl l). p)
	       c cls
    else
	let h = l/2 in
        let cls1 = head h cls
        and ((n,_,l).cls2) = tail h cls in
        let ll = "LC"@itos t@"_"@itos n in
        Mcompare r2 (const n).
        Mjcond eq (mstrlbl l).
        Mjcond gt ll.			-- CRAY and MIPS may not like this!
        makecmp t r2 cls1 (
        Mlabel ll.
	makecmp t r2 cls2 c)
#endif
and tagconv t = assocdef t [
(idlit "PAIR0", const O_PAIR0);
(idlit "PAIR1", const O_PAIR1);
(idlit "PAIR2", const O_PAIR2);
(idlit "PAIR3", const O_PAIR3);
(idlit "PAIR4", const O_PAIR4);
(idlit "TAG",   const O_TAG);
(idlit "TAG0",  const O_TAG0)
] (fail "tagconv")
and mgettagcase svf t g G max cl ldef V S =
	let cnt = length cl in
	let tags = map (\(v,n,_).ct (last (mconstr (fail "mgettagcase-vt") v n))) cl
		    where ct (CTAG _ _) = mtagtag
		       || ct (CNIL _ _) = mniltag
		       || ct (CPAIR _ n)= mpairtag n in
	if cnt = max & ~anysame tags & CaseTagOptim then
	    let (a._) = S in
	    let (c, d, uv, us, k) = M svf t 0 G [] [] in
	    let cl1 = combine(map thd cl, tags) in
	    let (c2, r) = (intoreg0 a [], 0) in	-- using register 0 improves code if there was a push just before
	    let rt = gareg V S in
	    let (c4, rn) = if FastTagOptim then ([], fail "mcase") else let d = gdreg V S in ([Mmove (regind rt oconstr) (reg d)], d) in
	    let cmpf (l, t) p = 
		    (if FastTagOptim then
			Mcompare (reg rt) t
		    else
			Mcompare (reg rn) (tagconv t)).
		    Mjcond eq (mstrlbl l). p
	    in
	    let c1 = reduce cmpf 
		            (Mjump(mstrlbl (if max=cnt then thd (last cl) else ldef)).c)
			    (butlast cl1)
	    in (Mcom "GETTAG; CASE...".c2@[Mmove (regind r 0) (reg rt)]@c4@c1, d, allWpush, Wuse.allWpush, 0)
	else
		mgettag svf t g (CASE max cl ldef.G) V S
end
