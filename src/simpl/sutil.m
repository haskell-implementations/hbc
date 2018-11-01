module -- sutil
#include "../expr/constr_t.t"
#include "../expr/constrfun.t"
#include "../expr/id_t.t"
#include "../expr/id.t"
#include "../expr/einfo_t.t"
#include "../ExprE/Expr_t.t"
#include "../misc/setofid.t"
#include "../misc/sort.t"
#include "../transform/hexpr.t"
export cval, value, isc, refc, xgetid, concstr;
rec
    cval (Cconstr _ _ _ n _ _) = n
and value (Econstr c []) = cval c
-- Don't say that strings are constants since they have a peculiar
-- representation.
and isc (Econstr c _) = ~ isstring c
||  isc _ = false
and Eis (Econstr c _) = isstring c
||  Eis _ = false
-- does not work yet because strings are expanded to c:cs.
and concstr (Eidapl ic [Econstr c1 []; Econstr c2 []]) & (eqid ic hiconc & isstring c1 & isstring c2) = 
    Econstr (mkstring (cname c1 @ cname c2)) []
||  concstr e = e
and refc = map (\x.(hd x, length x)) o groupsort Ilt

-- Special version of Egetid that makes sure that we don't substitute inside lambda by
-- counting everything twice there.
and
    xgetid (Eap e1 e2) = (xgetid e1)@(xgetid e2)
#if 0
*** Lambda lifting has already been done
||  xgetid (Elam i e) = dup (xgetid e)
#endif
||  xgetid (Ecase e cies de) =
	conc (xgetid e.xgetid de.map (\(_,_,e).xgetid e) cies)
||  xgetid (Elet _ ies e) =
	conc (xgetid e.map (\(_,e).xgetid e) ies)
||  xgetid (Evar i) = [i]
||  xgetid (Econstr _ es) = concmap xgetid es
||  xgetid (Efailmatch _) = []
||  xgetid (Ecfunction _ _) = []
||  xgetid (Eidapl i es) = i.concmap xgetid es
#if 0
*** Lambda lifting has already been done
||  xgetid (Elaml _ e) = dup (xgetid e)
#endif
||  xgetid (Einfo f e) = xgf f @ xgetid e
||  xgetid _ = fail "xgetid"
and xgf (spark is) = is
||  xgf _          = []

and dup is = let is' = Imk is in is'@is'
end
