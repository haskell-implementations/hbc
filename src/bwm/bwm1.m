module
#ifdef BWM
#include "../misc/misc.t"
#include "../misc/util.t"
#include "../misc/flags.t"
#include "../expr/constrfun.t"
#include "../expr/id_t.t"
#include "../expr/constr_t.t"
#include "../expr/id.t"
#include "../expr/ttype.t"
#include "../expr/einfo_t.t"
#include "../expr/booltree.t"
#include "../ExprE/Expr_t.t"
#include "../ExprE/apconv.t"
#include "../ExprE/Eprint.t"
/*#include "../llift/llift.t"*/
/*#include "../simpl/sutil.t"*/
#include "../transform/hexpr.t"
#include "../transform/exprs.t"
#include "../funnos.h"
export bwmfillcase;
rec bwmfillcase (Emodule i e dss) = Emodule i e (map (mapsnd fille) dss)

and fille (Einfo _ e) = fille e
||  fille (Elet r ies e) = Elet r (mapsnd fille ies) (fille e)
||  fille (Econstr c []) & (isstring c) = str2l (cname c)
||  fille (Econstr c es) = Econstr c (map fille es)
||  fille (Eidapl i es) = Eidapl i (map fille es)
||  fille (Elaml is e) = Elaml is (fille e)
||  fille (e as Efailmatch 0) = e
||  fille (xx as Ecase e cies ed) = 
    if ncon cies > 50 then
	if isfl cies then
	    iftrans (fille e) ((mapthd (fillc (fille ed)) cies)) ed
	else
	    fail ("Cannot convert case expression "@pr xx)
    else
	Ecase (fille e) (mapthd (fillc (fille ed)) (expc 0 (typ xx cies) cies)) (Efailmatch 0)
||  fille e = fail ("fille "@pr e)
and fillc ed (Efailmatch 1) = ed
||  fillc ed (Ecase e cies (Efailmatch 1)) = fille (Ecase e cies ed)
||  fillc _  e = fille e
and expc n (m,t) _ & (n >= m) = []
||  expc n mt ((tr as (c, is, e)).cies) & (cno c = n) = tr.expc (n+1) mt cies
||  expc n (mt as (_,t)) cies = mknc n t.expc (n+1) mt cies
and mknc n t = let c = nth_constr n t in (c, rept (carity c) dummyid, Efailmatch 1)
and typ xx ((c, _, _)._) = (nconstrs c, ctinfo c)
and ncon ((c, _, _)._) = nconstrs c
and isfl (((Cconstr _ _ (mktinfo _ _ _ x _ _ _ _) _ _ _), _, _)._) = x
and str2l [] = Econstr hcnil []
||  str2l (c.cs) = Econstr hccons [Econstr (xcchar c) []; str2l cs]
and iftrans e cies d =
    reduce (eif e (Efailmatch 0)) d cies
and eif e d (c,[],t) f = Ecase (Eidapl peq [e; Econstr c []]) [(hctrue,[],t); (hcfalse,[],f)] d

and peq = (mkid Feq "Peq" pre2 noorigname
	   where rec pre2  = idi_var (var_pre (finfo 2 [] (v01, v01)  2 None)) Onotype None
	          and v01 = btands [v 0; v 1]
	          and v n = btors [btvar n])
#else
export ;
dummy=0
#endif
end
