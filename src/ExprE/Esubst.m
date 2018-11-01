module -- Esubst
--
-- Substitute an expression for an identifier in a given expression.
--
#include "../expr/id.t"
#include "Expr_t.t"
#include "../misc/misc.t"
#include "../expr/einfo_t.t"
#include "../misc/util.t"
#include "Eprint.t"

export Esubst, Ealphasubst;

    Esubst s i e = (S e
where rec
    S (Eap e1 e2) = Eap (S e1) (S e2)
||  S (Elam i1 e) = Elam i1 (S e)
||  S (Ecase e1 cl e2) = Ecase (S e1) (mapthd S cl) (S e2)
||  S (Elet r dl e) = Elet r (mapsnd S dl) (S e)
||  S (Evar ii) = if eqid i ii then s else Evar ii
||  S (Emodule i expl dl) = Emodule i expl (map (mapsnd S) dl)
||  S (Econstr c el) = Econstr c (map S el)
||  S (Efailmatch n) = Efailmatch n
||  S (e as Ecfunction _ _) = e
||  S (Eidapl ii es) = 
	if eqid i ii then 
	    case s in 
	       Eidapl i1 ses : Eidapl i1 (ses @ map S es)
	    || _ : fail "Esubst"
	    end
	else
	    Eidapl ii (map S es)
||  S (Elaml is e) = Elaml is (S e)
||  S (Einfo f e) = Einfo (F f) (S e)
||  S _ = fail ("Esubst "@pr e)
and
    F (spark is) = spark (map (\i1.if eqid i i1 then let (Evar ni)=s in ni else i1) is)
||  F f = f)
and
    Ealphasubst iis e = (S e
where rec
    S (Eap e1 e2) = Eap (S e1) (S e2)
||  S (Elam i1 e) = Elam (A i1) (S e)
||  S (Ecase e1 cl e2) =
	Ecase (S e1) (map (\(c,is,e). (c, map A is, S e)) cl) (S e2)
||  S (Elet r dl e) = Elet r (map (\(i,e).(A i, S e)) dl) (S e)
||  S (Evar ii) = Evar (A ii)
||  S (Econstr c el) = Econstr c (map S el)
||  S (Efailmatch n) = Efailmatch n
||  S (e as Ecfunction _ _) = e
||  S (Eidapl ii es) = Eidapl (A ii) (map S es)
||  S (Elaml is e) = Elaml (map A is) (S e)
||  S (Einfo f e) = Einfo (F f) (S e)
||  S _ = fail "Ealphasubst"
and
    F (spark is) = spark (map A is)
||  F f = f
and
    A i = assocdefeq eqid i iis i)
end
