module -- constrfun
-- selector functions for the Constr type.
#include "../misc/util.t"
#include "types_t.t"
#include "constr_t.t"
#include "id_t.t"
#include "id.t"
#include "ttype_t.t"
#include "ttype.t"
#include "../transform/hexpr.t"
export cname, ctype, ctinfo, cno, cargs, tctype, nconstrs,
	nth_constr, mkstring, isstring, constrtype, carity, cexist,
	mksfloat, mkdfloat, mkinteger, issfloat, isdfloat, isinteger, mkrational, isrational;
rec
    cname  (Cconstr s _ _ _ _ _) = s
and ctype  (Cconstr _ t _ _ _ _) = t
and ctinfo (Cconstr _ _ i _ _ _) = i
and cno    (Cconstr _ _ _ n _ _) = n
and cexist (Cconstr _ _ _ _ e _) = e
and cargs  (Cconstr _ _ _ _ _ a) = a

and carity c = length (cargs c)

and tctype (mkconstr c _) = ctype c

and nconstrs (Cconstr _ _ (mktinfo _ n _ _ _ _ _ _) _ _ _) = n

-- make the nth constructor given type info.
and nth_constr n (ti as mktinfo t _ _ _ a _ _ _) =
	let (mkcons i e l _) = select (n+1) a in
	Cconstr (idtostr i) t ti n e [(t,b) ;; (t,b,sel) <- l]

and ITstring = mktinfo Tstring (-1) false false [] false false None
and mkstring s = Cconstr s Tstring ITstring (-1) (false,[],[]) []
and isstring (Cconstr _ _ (mktinfo _ (-1) _ _ _ _ _ _) _ _ _) = true
||  isstring _ = false
and mkdfloat s = Cconstr s Tdfloat (mktinfo Tdfloat (-2) false true [] false false None) (-2) (false,[],[]) []
and isdfloat (Cconstr _ _ (mktinfo t _ _ _ _ _ _ _) _ _ _) = eqtype t Tdfloat
||  isdfloat _ = false
and mksfloat s = Cconstr s Tsfloat (mktinfo Tsfloat (-5) false true [] false false None) (-5) (false,[],[]) []
and issfloat (Cconstr _ _ (mktinfo t _ _ _ _ _ _ _) _ _ _) = eqtype t Tsfloat
||  issfloat _ = false
and mkinteger s = Cconstr s Tinteger (mktinfo Tinteger (-3) false false [] false false None) (-3) (false,[],[]) []
and isinteger (Cconstr _ _ (mktinfo t _ _ _ _ _ _ _) _ _ _) = eqtype t Tinteger
||  isinteger _ = false
and mkrational s = Cconstr s Trational (mktinfo Trational (-4) false false [] false false None) (-4) (false,[],[]) []
and isrational (Cconstr _ _ (mktinfo t _ _ _ _ _ _ _) _ _ _) = eqtype t Trational
||  isrational _ = false
and constrtype (Cconstr s t _ no _ _) = 
        assocdefeq eqid (tname t) [(hiInt, Gint); (hiChar, Gchar); (hiDFloat, Gdfloat s); (hiSFloat, Gsfloat s); (hiInteger, Ginteger s)] 
	     (if no = -1 then
		Gstring s
	      else
	      	Gtype)
end
