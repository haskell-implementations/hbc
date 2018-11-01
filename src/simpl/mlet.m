module -- mlet
#include "../expr/id_t.t"
#include "../expr/id.t"
#include "../expr/einfo_t.t"
#include "../ExprE/Expr_t.t"
#include "../ExprE/Eprint.t"
#include "../misc/misc.t"
#include "../misc/util.t"
#include "../misc/flags.t"
#include "sutil.t"
#include "simpl.t"
/*import simpl: *a;*/

export mlet, mletr;
rec
    issimpl (Eidapl _ []) = true
||  issimpl (Econstr _ []) = true
||  issimpl (Einfo inline _) = true
||  issimpl e = false
and dropinline (Einfo inline e) = e
||  dropinline e = e
-- Estimate if an expanded function call is simpler than the closure.
-- This only an heuristic approximation and it is not critical.
#if 0
*** Lambda lifting has already been done
and issimplfun (Elaml is e) = (false & size e <= 1 + 2 * length is		-- TURN THIS OFF!
    where rec size (Eidapl _ es) = 1 + 2 * Sum (map size es)
	   || size (Econstr _ es) = 1 + Sum (map size es)
	   || size _ = 10000)
||  issimplfun _ = false
#endif
and recok i (Eidapl i1 []) = i ~= i1
||  recok _ _ = true

and recdef u defs =
	revitlist (\(i,e).\(u,ds).let (u',e') = simpl u ds e in (u', (i, e').ds)) defs (u, [])

and ml u rc v d ies e =
	let (u', (nies, d0)) = reduce f (u, ([],[])) ies 
	    where f (i,e) (u, r as (bs, ds)) =
--trace ("ml-f: "@prid i@" "@pr e@" "@itos (assocdefeq eqid i v 0)) (
			let (u', ne) = simpl u (if rc then ds@d else d) e in
			case assocdefeq eqid i v 0 in
			    0 : (u', r)
			||  n : if n = 1 & ~rc & ~NoSubst |
				   issimpl ne & (~rc /*| recok i e*/) /*|
				   ~rc & issimplfun ne*/ then
					(u', (bs, (i, dropinline ne).ds))
				else
					(u', ((i, dropinline ne).bs, ds))
			end
--)
	in
	-- for recursive definitions simpl will be n^2!!
	let (u'', d1) = if rc then recdef u' d0 else (u', d0) in
	let (u''', mies) =
		if rc then
			mapstate (\u.\(i,e).let (u',e') = simpl u d1 e in (u',(i,e'))) u'' nies
		else 
			(u'', nies) in
	let (u'''', ne) = 
		case e in
		   Elet r ies e : ml u''' r v (d1@d) ies e
		|| _ : simpl u''' (d1@d) e
		end in
	if mies = [] then
		(u'''', ne)
	else
		(u'''', Elet rc mies ne)
and mlet u d ies e = ml u false (refc (xgetid e)) d ies e
and mletr u d ies e = ml u true (refc (concmap xgetid (e.map snd ies))) d ies e
end
