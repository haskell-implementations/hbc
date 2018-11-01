module
#include "../misc/misc.t"
#include "mcodedef_t.t"
#include "../Gcode/Gcodedef_t.t"
#include "../Gcode/Gprint.t"
#include "Wuse.t"
#include "mutil.t"
#include "mutil1.t"
#include "mregs.t"
#include "mstrlbl.t"
#include "mstrid.t"
#include "handmade.mcode.t"
#include "mvectable.t"
#include "mprint.t"

export malloc, mrecblock;
rec malloc svf t G 0 V S = M svf t 1 G V S
||  malloc (strs,vecs,fstr) t G m V S =
        let nsize = hpmsize 3 					    in
        let nstrs = adds [fstr;"Hole"] strs                         in
        let snumh = snum "Hole" nstrs                               in
        let vec   = (snum fstr nstrs, snumh, snumh)                 in
        let nvecs = addv vec vecs                                   in

	let rec (c, d, uv, u.us, k) = M (nstrs, nvecs, fstr) t 1 (ALLOC (m-1).G) V S1
	and     (c1, S1) = newheapS nsize u S c
	in (	Mcom "ALLOC 1".
		Mmove (idlit "HOLE") tohp.
		Mmove (hprel 2) hp.
                hpmoves vec     tohp @
                c1, d, uv, us, k+nsize)

and mrecblock svf t g gss G V S =
#if 1
    let m = length gss in
    let gss' = map (@[POP 1]) gss in
    let rec (cs, S') = reduce (\(u, n).\(c,S).newhS n u S c) ([], S) (combine(us, xs))
        and (c, d, uv, usm, k) = M svf t 1 (conc gss' @ G) V S'
        and xs = reverse (map2 (-) (tl (scanl (+) 0 (map gsize gss'))) (map (gsize1 o last) gss))
        and us = head m usm
        and us' = tail m usm
        and hS = rept m (hprel 0) @ S
	-- Use M to just compute the size of each code block.
	-- This is a waste, but easy.
        and gsize gs = let (_,_,_,_,k) = M ([],[],"?") 0 1 gs V S' in if k = 0 then fail "gsize=0" else k
    in  (Mcom ("RECBLOCK "@"uxs="@show_list (show_pair(show_use,show_int)) (combine(reverse us,xs)) @ ", S'="@prstk 5 S').
	 reverse cs@c, d, uv, us', k)
#else
    M svf t g (flatrecblock gss @ G) V S
#endif

and flatrecblock gss = let m = length gss in ALLOC m . concmap2 (\g.\x. g @ [ UPDATE Gbother (m-x) ]) gss (from 0)
and scanl f q xs	=  q . case xs in
				  []   : []
			       || x.xs : scanl f (f q x) xs
			       end
and gsize1 (PUSH _) = 0
||  gsize1 (PUSHGLOBAL _) = 0
||  gsize1 (CNIL _ _) = 0
||  gsize1 (CBASIC _) = 0
||  gsize1 (CSTRING _) = 0
||  gsize1 (CPAIR _ _) = hpmsize 3
||  gsize1 (CTAG _ _) = hpmsize 3
||  gsize1 (CVEK _ i) = hpmsize (i+2)
||  gsize1 (MKAP _) = hpmsize 3
||  gsize1 (MKCAP _) = hpmsize 3
||  gsize1 (MKAPLV _ i) = hpmsize (i+2)
||  gsize1 _ = fail "gsize1"

and newhS n (Wpush)  S c = (Mmove (hprel n) pushS.c, popS.S)
 || newhS n (Wreg r) S c = (Mmove (hprel n) (reg r).c, (reg r).S)
 || newhS n (Wuse)   S c = (c , hprel n.S)

and show_use Wpush = "Wpush"
||  show_use (Wreg r) = "Wreg "@itos r
||  show_use Wuse = "Wuse"
and prstk n S = show_list pamode (head n S)
end

