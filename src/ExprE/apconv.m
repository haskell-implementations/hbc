module -- apconv
--
-- Transform application and lambda chains to simplify lambda lifting.
--
#include "../misc/misc.t"
#include "../expr/einfo_t.t"
#include "../expr/id_t.t"
#include "../expr/id.t"
#include "Expr_t.t"
#include "Eprint.t"
export apconv, transap;
rec

    transap [] e [] = e
||  transap is e [] = Elaml is e
||  transap [] e es = apchain es e
||  transap is e es = 
    let es' = tail (length is) es in
    let is' = tail (length es) is in
    transap is' (Elet false (combine(is,es)) e) es'
and
    apconv e = apcnv e
and
    -- remove type restrictions, they are not needed any more.
    minfo (restr _ _) e = e
||  minfo f e = Einfo f e
and apcnv (Eap f a) = apchain [a] f
||  apcnv (Elam i e)= lamchain [i] [] e
||  apcnv (Ecase e1 pl de) =
	Ecase (apcnv e1) (mapthd apcnv pl) (apcnv de)
||  apcnv (Elet r dl e) = Elet r (mapsnd apcnv dl) (apcnv e)
||  apcnv (Emodule i exp dl) = Emodule i exp (map (mapsnd apcnv) dl)
||  apcnv (Econstr c el) = Econstr c (map apcnv el)
||  apcnv (Evar i) = Eidapl i []
||  apcnv (Efailmatch n) = Efailmatch n
||  apcnv (e as Ecfunction _ _) = e
||  apcnv (Einfo f e) = minfo f (apcnv e)
||  apcnv (Elaml is e) = lamchain is [] e
||  apcnv (Eidapl i es) = Eidapl i (map apcnv es)
||  apcnv e = fail ("apcnv: "@pr e)
and
    lamchain il al (Elam i e) = lamchain (il@[i]) al e
||  lamchain il al (Elaml is e) = lamchain (il@is) al e   
||  lamchain il al e = transap il (apcnv e) (map apcnv al) -- needed because of call in transap
and
    apchain al (Eap f a) = apchain (a.al) f
||  apchain al (Elam i e)= lamchain [i] al e
||  apchain al (Ecase e1 pl de) = Ecase (apcnv e1) (mapthd (apchain al) pl) (apchain al de)
||  apchain al (Elet r dl e) = Elet r (mapsnd apcnv dl) (apchain al e)
||  apchain al (Evar i) = Eidapl i (map apcnv al)
||  apchain al (Efailmatch n) = Efailmatch n
||  apchain al (Einfo f e) = minfo f (apchain al e)
||  apchain al (Eidapl i es) = Eidapl i (es@al)            -- needed because of call in transap
||  apchain al (Elaml is e) = lamchain is al e             -- needed because of call in transap
||  apchain al _ = fail "strange apchain expr"
end
