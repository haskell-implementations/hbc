module -- failcase
#include "../expr/id_t.t"
#include "../expr/id.t"
#include "../expr/constr_t.t"
#include "../expr/ttype_t.t"
#include "../expr/einfo_t.t"
#include "../expr/types_t.t"
#include "../expr/constrfun.t"
#include "../expr/ttype.t"
#include "../expr/subst.t"
#include "../expr/pprint.t"
#include "hexpr.t"
#include "misc.t"
#include "../expr/tinfo.t"
export fmkcase, tfail;
rec

    nfail = (dollar, mkfailmatch 0)
and tfail s = mkap (mkident hifail) (mkconstr (mkstring s) [])

and allcases [] = false
||  allcases ((mkconstr c _).l) = nconstrs c = length l + 1

-- Compute how many fails are jumping out of an expression.
-- This is somewhat kludgy.
and nfails (mkcase _ ((_, e)._)) = nfails e
||  nfails (mkfailmatch n) = n
||  nfails _ = 0

and repfail d (mkcase e ((di, de).pes)) = mkcase e ((di, repfail d de).pes)
||  repfail d (mkfailmatch 1) = d
||  repfail _ e = e
#if 0
and isfailmatch (mkfailmatch _) = true
||  isfailmatch _ = false
#endif

and insertview e [] = e
||  insertview e ((mkconstr c _, _)._) =
	case get_view (ctinfo c) in
	   Some vw : mkap (mkident (getviewfun vw)) e
	|| None : e
	end
||  insertview e (_.pbs) = insertview e pbs
and getviewfun (mkid _ _ (idi_view _ i _) _) = i

and fmk i0 ((def as (_,d)).pes) =
    let i = insertview i0 pes in
	if  allcases (map fst pes) then
		let n = Sum (map (nfails o snd) pes) in
		if n = 0 then
			mkcase i (nfail.pes)
		else if n = 1 & nfails d = 0 then
			mkcase i (nfail.mapsnd (repfail d) pes)
		else
			mkcase i (def.pes)
	else
	    mkcase i (def.pes)

and extend [d] = [d]
||  extend (d.pes) =
	let ((mkconstr c _, _).l) = pes in
	let n = length pes
	and m = nconstrs c in
	if n < m & n+1 >= m then
		d.(f (reverse (count 0 (m-1))) pes where rec
		    f [] _ = []
		 || f (n.ns) ((pe as (mkconstr c _, _)).pes) & (n = cno c) =
			pe.f ns pes
		 || f (n.ns) pes = let c' = nth_constr n (ctinfo c) in
(mkconstr c' (map (\_.dollar) (cargs c')), mkfailmatch 1).f ns pes)
	else
		d.pes

and opt i (d as [(mkident di, de)]) = subst i di de
||  opt i ((mkident di, de).cs) = fmk i ((dollar, subst i di de).cs)
||  opt i pes = fmk i pes

and fmkcase i pes = opt i (extend pes)
end
