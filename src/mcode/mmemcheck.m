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
#include "mprint.t"
#include "handmade.mcode.t"
#include "machine.tt"
#include "limit.h"
export mmemcheck, allusedregs, saverestore;
rec
mmemcheck svf t G V S =
    if ~X4 then
	let rec (c, d, uv, us, k) = M svf (t+2) 1 G allpopV allpopS
--                                        label
	and ts = tmplbl t
	in
	    if k ~= 0 then
	       (Mcom ("MEMCHECK "@itos k).
	        (if k <= HCLAIM then
			(if MemCheckStat then
			    let ra = gareg V S in
				Mmove (idlit "_memcheckhisto") (reg ra).
				Mop2 add (const 1) (regind ra k). []
			else
				[]) @
			Mcompare hp (glob "_ehp") .
			(if FastGCCheck then
			    let ts1 = tmplbl (t+1) in
			    [Mjcond geheap ts;
			     Mtext1;
			     Mlabel ts;
			     Mcall "GARB";
			     Mjump ts1;
			     Mtext;
			     Mlabel ts1]
			 else
			    [Mjcond ltheap ts;
			     Mcall "GARB";
			     Mlabel ts]) @
			c
		 else
			(if MemCheckStat then
				Mop2 add (const 1) (glob "_memcheck_gthclaim").
				[]
			else
				[]) @
			Mmove (const (k-HCLAIM)) argcreg .
			Mcall "NGARB" .
			c
		 ),
			d, allWpush, allWpush, 0)
	    else
--		    (Mcom ("MEMCHECK 0").c, d, uv, us, 0)
		-- It's wasteful to call M again, but needed to get things right
		let (c, d, uv, us, k) = M svf t 1 G V S
		in (Mcom ("MEMCHECK 0").c, d, uv, us, k)
   else
		let (c, d, uv, us, k) = M svf (t+2) 1 G V S
	        and ts = tmplbl t in
		let (ads as (aregs, dregs)) = allusedregs S V [] in
		let (sc, rc) = saverestore ads
		in ((if k ~= 0 then
			 Mcom ("MEMCHECK "@itos k@" "@prstk 5 S@";"@prstk 5 V@";"@show_list itos aregs@";"@show_list itos dregs).
			    let ts1 = tmplbl (t+1) in
			    [Mcompare hp (glob "_ehp");
			     Mjcond geheap ts;
			     Mtext1;
			     Mlabel ts]@
                             sc@
			     Mcall "GARB".
			     rc@
			     [ Mjump ts1;
			     Mtext;
			     Mlabel ts1] @
			 c
		     else
			 Mcom "NOCHECK".c), d, uv, us, k)
and allusedregs S V no = 
        let aregs = difference (mkset (allregs S @ alliregs V)) no
        and dregs = mkset (allrregs V)
	in  (aregs, dregs)
and
    allregs (reg p.S) = p . allregs S
||  allregs (regind p _.S) = p . allregs S
||  allregs (regrel p _.S) = p . allregs S
||  allregs (popS._) = []
||  allregs (popV._) = []
||  allregs [] = []
||  allregs (_.S) = allregs S
and
    allrregs (reg p.S) = p . allrregs S
||  allrregs (popS._) = []
||  allrregs (popV._) = []
||  allrregs [] = []
||  allrregs (_.S) = allrregs S
and
    alliregs (regind p _.S) = p . alliregs S
||  alliregs (regrel p _.S) = p . alliregs S
||  alliregs (popS._) = []
||  alliregs (popV._) = []
||  alliregs [] = []
||  alliregs (_.S) = alliregs S

and saverestore (ar,vr) = (map (\r.Mmove (reg r) pushS) ar @ map (\r.Mmove (reg r) pushV) vr,
                           map (\r.Mmove popS (reg r)) (reverse ar) @ map (\r.Mmove popV (reg r)) vr)
end


