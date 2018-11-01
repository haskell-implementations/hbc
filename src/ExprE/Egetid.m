module -- Egetid
--
-- Get ids from an expression (note: not only free, but in a renamed Expr
-- it usually doesn't matter).
-- The returned list contains duplicates to enable refernce counts.
--
#include "Expr_t.t"
#include "../expr/einfo_t.t"
export Egetid, Egetlamid;
rec
    Egetid (Eap e1 e2) = (Egetid e1)@(Egetid e2)
||  Egetid (Elam i e) = Egetid e
||  Egetid (Ecase e cies de) =
	conc (Egetid e.Egetid de.map (\(_,_,e).Egetid e) cies)
||  Egetid (Elet _ ies e) =
	conc (Egetid e.map (\(_,e).Egetid e) ies)
||  Egetid (Evar i) = [i]
||  Egetid (Econstr _ es) = concmap Egetid es
||  Egetid (Efailmatch _) = []
||  Egetid (Ecfunction _ _) = []
||  Egetid (Eidapl i es) = i.concmap Egetid es
||  Egetid (Elaml _ e) = Egetid e
||  Egetid (Einfo f e) = Egf f @ Egetid e
||  Egetid _ = fail "Egetid"
and
    Egf (spark is) = is
||  Egf _          = []
and
-- Get all bound identifiers
    Egetlamid (Eap e1 e2) = (Egetlamid e1)@(Egetlamid e2)
||  Egetlamid (Elam i e) = i.Egetlamid e
||  Egetlamid (Ecase e cies de) =
	conc (Egetlamid e.Egetlamid de.map (\(_,is,e).is@Egetlamid e) cies)
||  Egetlamid (Elet _ ies e) =
	conc (Egetlamid e.map (\(i,e). i.Egetlamid e) ies)
||  Egetlamid (Evar i) = []
||  Egetlamid (Econstr _ es) = concmap Egetlamid es
||  Egetlamid (Efailmatch _) = []
||  Egetlamid (Ecfunction _ _) = []
||  Egetlamid (Eidapl i es) = concmap Egetlamid es
||  Egetlamid (Elaml is e) = is @ Egetlamid e
||  Egetlamid (Einfo f e) = Egetlamid e
||  Egetlamid _ = fail "Egetlamid"
end

