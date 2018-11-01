module -- tchk
--
-- typechecker
--
#include "../expr/ttype_t.t"
#include "../expr/id_t.t"
#include "../expr/id.t"
#include "../expr/impexp_t.t"
#include "../expr/ttype.t"
#include "../expr/types_t.t"
#include "../expr/error.t"
#include "../transform/misc.t"
#include "../misc/flags.t"
#include "../misc/misc.t"
#include "../type/subst_t.t"
#include "check.t"
#include "../type/prefix.t"
#include "../type/unify.t"
#include "../type/subst.t"
#include "xsubst.t"

export etcheck ;

rec
    normt ot =
        let t = normtype ot in
	Ohastype t (getTvars t) None

-- etcheck : ERRMAP -> Texpr -> N -> (List Texpr x ? x Subst x Texpr x Prefix x N)
and etcheck errmap (e0 as mkmodule mid fi impl (Some exps) b) u0 = 
    let u = u0+1 in 
     if ~TypeCheck then 
	 ([],[],emptyTR,e0,startpre,u)
     else 
	let ip = (concmap imp impl
		  where imp (mkimpid i t _ _) = [(i,(t,getTvars t))]
		     || imp _                 = []) in
        let (es,vb,S,u') = Wb (addgens ip startpre) b u in
	let pre = addpre vb startpre in
	case S in
	   (bad s) : (es, [findloc errmap " in " s],emptyTR,e0,TRprefix S pre,u')
        || _ :
	-- check if main program
	    case exps in
	      [mkexpid (i as mkid n "Pmain" (idi_var vi _ _) on)] :
		let npre = addpre (mapsnd (TRtype S) vb) startpre in
		let (ty, _) = pfind i npre in
		let U = Unify ty (Tarr Tstring 
				       (if nuflag then Tstring else mktvar u0)
				 ) in
		let pre = addpre [(i,TRtype U ty )] startpre in
		case U
		  in (bad s) :
		      (es,[findloc errmap " in " s], emptyTR, e0, pre, u)
		  || _ : 
		      let t = normt (TRtype U ty) in
		      (es,
                       [],
		       S,
		       mkmodule mid fi impl
				(Some [mkexpid (mkid n "Pmain" (idi_var vi t None) on)]) b ,
		       pre ,
		       u' 
		      )
		end
	      || _ :
		case ynsplit (map (addexptype pre) exps) [] [] in
		 (nexps,[]) :
		     (es,[],S, (mkmodule mid fi impl (Some nexps) b), pre, u')
		||  (nexps, nos) :
		     (es,[findloc errmap " in " ["Bad overloading in exports: "@mixmap oprid nos ", "]], emptyTR, e0, pre, u')
		end
	    end
	end

and addexptype npre (mkexpid (i as mkid n s (idi_var vi _ _) on)) =
    let (t, vs) = pfind i npre in
    if difference (getTvars t) vs = [] | Relax then
	Yes (mkexpid (mkid n s (idi_var vi (normt t) None) on))
    else
	No i
||  addexptype _ e = Yes e					
end
