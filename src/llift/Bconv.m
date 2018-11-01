module
#include "../expr/id_t.t"
#include "../expr/id.t"
#include "../expr/einfo_t.t"
#include "../ExprE/Expr_t.t"
#include "../misc/misc.t"
#include "../misc/flags.t"
#include "../funnos.h"
#include "para.t"
export Bconv;
rec
    nid n = mknewid "V" n
and
    Bconv (Emodule i exps ds) u =
	let (ds', u1) = Umap (Umap f) ds u
	where f (i, Elaml is e) u = let (e', u') = E e u in ((i, Elaml is e'), u')
	||    f (i, e) u = let (e', u') = E e u in ((i, e'), u')
	in (Emodule i exps ds', u1)
and
    E (Ecase e ps ed) u = 
	let (e', u1) = E e u in
	let (ps', u2) = Umap (\(c,is,e).\u.let (e', u') = E e u in ((c, is, e'), u')) ps u1 in
	let (ed', u3) = E ed u2 in
	(ecase e' ps' ed', u3)
||  E (e as Efailmatch _) u = (e, u)
||  E (e as Ecfunction _ _) u = (e, u)
||  E (Elet re ds e) u =
	let (ds', u') = Umap (\(i,e).\u.let (e', u')=C e u in ((i, e'), u')) ds u
	in let (e', u'') = E e u' 
	in (Elet re ds' e', u'')
||  E (Econstr c es) u = let (es', u') = Umap C es u in (Econstr c es', u')
||  E (e as Eidapl i _) u & (id_is_predef i & ~specialpre i) = B e u
||  E (Eidapl i es) u = let (es', u') = Umap C es u in (Eidapl i es', u')
||  E (Einfo i e) u = let (e', u') = E e u in (Einfo i e', u')
-- Eap, Elam, Evar, Emodule, Elaml cannot occur

-- Make sure we don't get any let expressions as the scrutinized expression (it
-- gives bad code).
and ecase (Elet r ds e) ps de = Elet r ds (ecase e ps de) 
||  ecase (Einfo strict (Elet r ds e)) ps de = Elet r ds (ecase e ps de) 
||  ecase (Einfo (i as doeval _) (Elet r ds e)) ps de = Elet r ds (Einfo i (ecase e ps de))
||  ecase e ps de = Ecase e ps de
and
    C (Elet re ds e) u =
	let (ds', u') = Umap (\(i,e).\u.let (e', u')=C e u in ((i, e'), u')) ds u
	in let (e', u'') = C e u' 
	in (Elet re ds' e', u'')
||  C (Econstr c es) u = let (es', u') = Umap C es u in (Econstr c es', u')
||  C (Eidapl i es) u = let (es', u') = Umap C es u in (Eidapl i es', u')
||  C (Einfo strict e) u = let (e', u') = E e u in (Einfo strict e', u')
||  C (Einfo i e) u = let (e', u') = C e u in (Einfo i e', u')
||  C (e as Ecfunction _ _) u = (e, u)
-- Eap, Elam, Evar, Emodule, Elaml, Ecase, Efailmatch cannot occur
and
    B e u =
	let ((bs, is, e), u') = rep e u in
--	if Parallel then
--		(paraspark bs is e, u')
--	else
		(edoeval (elet false bs e) is, u')
and
    rep (Eidapl i es) u & (id_is_predef i & ~specialpre i) =
	let (bsses, u') = Umap rep es u in
	let (bss, iss, es') = split3 bsses in
	((conc bss, reduce union [] iss, Eidapl i es'), u')
||  rep (e as Econstr _ []) u = (([], [], e), u)
||  rep (e as Ecfunction _ _) u = (([], [], e), u)
||  rep (e as Einfo noeval _) u = (([], [], e), u)
||  rep (Einfo strict e) u = rep e u
||  rep (e as Eidapl i []) u = (([], [i], Einfo noeval e), u)
||  rep e u =
	let (e', u') = E e (u+1) in
	let i = nid u in
	(([(i, Einfo strict e')], [], Einfo noeval (Eidapl i [])), u')
and elet r [] e = e
||  elet r ds e = Elet r ds e
and edoeval e [] = e
||  edoeval e is = Einfo (doeval is) e
and split3 [] = ([], [], [])
||  split3 ((a, b, c).xs) =
	let (aas, bbs, ccs) = split3 xs
	in (a.aas, b.bbs, c.ccs)
end
