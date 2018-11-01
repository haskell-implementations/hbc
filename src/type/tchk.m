module -- tchk
--
-- typechecker
--
#include "../expr/ttype_t.t"
#include "../expr/id_t.t"
#include "../expr/id.t"
#include "../expr/ttype.t"
#include "../expr/impexp_t.t"
#include "../expr/error.t"
#include "../ExprE/Expr_t.t"
#include "../ExprE/Eprint.t"
#include "../ExprE/addspec.t"
#include "../ExprE/addrestr.t"
#include "../transform/misc.t"
#include "../misc/flags.t"
#include "../misc/misc.t"
#include "subst_t.t"
#include "check.t"
#include "hcheck.t"
#include "prefix.t"
#include "unify.t"
#include "subst.t"

export tcheck;

rec
    normt ot =
        let t = normtype ot in
	Ohastype t (getTvars t) None

and Wdlx p b defs u = 
	if Curry then
	    let rec (r as (St, _, _, _)) = Wdlh St p b defs u in r
	else
	    Wdll p b u

and redowithspec n em defs e0 r ops =
        if n = 0 then r else
	    case r in
		([], S, E, p, u') :
		    let (ps, u'') = collectspec S E u' in
		    if null ps then
			r
		    else
		    let ps = ps@ops in
		    let (E', u''') = addautospec ps e0 u'' in
		    let (E'', u'''') = addrestr (map fst ps) E' u''' in
--trace ("ps="@show_list (show_pair(prid,show_list (show_pair(prid,prttype)))) ps @"E''="@pr E'')
--trace ("autospecs="@show_list (show_pair(prid,show_list (show_pair(prid,prttype)))) ps)
(
		    let r' = tcheck0 em E'' (ps,defs) u''''
		    in  redowithspec (n-1) em defs e0 r' ps
)
	    ||  _ : r
	    end

and tcheck em e0 defs u0 =
	let r = tcheck0 em e0 ([],defs) u0
	in  redowithspec itercnt em defs e0 r []
and itercnt =
	if AutoSpecialize then
	    if Optimize then /*2*/1 else 1		-- 2 causes problems, double spec prefix (forgot to remove restr?)
	else
	    0

and tcheck0 errmap (e0 as Emodule mid exps b) defs u0 = 
    let u = u0+1 in
     if ~TypeCheck then
	([], emptyTR, e0, startpre, u)
     else
	let! (S, npre, u', b') = Wdlx startpre b defs u in
	case S in
	   (bad s) : ([findloc errmap " in " s], emptyTR, e0, npre, u')
        || _ :
	-- check if main program
	    case exps in
	       [mkexpid (i as mkid n "Pmain" (idi_var vi _ _) on)] :
		let! (ty, _) = pfind i npre in
		let U = Unify ty (Tarr Tstring (if nuflag then Tlist Tchar else mktvar u0)) in
		case U in
		   (bad s) : ([findloc errmap " in " s], emptyTR, e0, npre, u')
		|| _ : let t = normt (TRtype U ty) in 
		       ([], pruneTR u S, Emodule mid [mkexpid (mkid n "Pmain" (idi_var vi t None) on)] b', npre, u')
		end
	    || _ :
		case ynsplit (map (addexptype npre) exps) [] [] in
		    (nexps, []) : ([], pruneTR u S, (Emodule mid nexps b'), npre, u')
		||  (nexps, nos) : ([findloc errmap " in " ["[62] Bad overloading in exports: "@mixmap oprid nos ", "]], emptyTR, e0, npre, u')
		end
	    end
	end

and addexptype npre (mkexpid (i as mkid n s (idi_var vi _ osel) on)) =
    let (t, vs) = pfind i npre in
    if ~ innongen i npre | null (difference (getTvars t) vs) then
	Yes (mkexpid (mkid n s (idi_var vi (normt t) osel) on))
    else
	No i
||  addexptype _ e = Yes e					
end
