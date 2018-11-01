module
#include "../expr/id.t"
#include "../expr/id_t.t"
#include "../expr/impexp.t"
#include "../expr/pragma_t.t"
#include "../misc/misc.t"
#include "../misc/util.t"
#include "../main/topexprs.t"	/* pragmas */
#include "Egetid.t"
#include "Expr_t.t"
#include "Eprint.t"
#include "unrec.t"
export remunused;
rec remunused (Emodule i e dss) = 
	let (dss', _) = Umap remds (reverse dss) (map (getvw o expid) e @ concmap specid (concmap snd pragmas))
	in  Emodule i e (filter (not o null) (reverse dss'))
and remds ds is =
	if ~ exists (keepid is o fst) ds then
	    ([], is)
	else
	    let ds' = mapsnd reme ds in
	    (ds', is @ mkseteq eqid (concmap (Egetid o snd) ds'))
and specid (Pspecialize i [t]) = [i]
||  specid _ = []
and keepid is i = special i | member eqid i is
and special i = 
	case idtostr i in
	   'M'.'M'._ : true
	|| 'D'.'D'._ : true
	|| 'V'.'V'._ : true
	||  _ : false
	end
and getvw i =
    case i in
       mkid _ _ (idi_view _ i _) _ : i
    || i : i
    end
and reme e = (S e 
where rec
    S (Ecase e1 cl e2) = Ecase (S e1) (mapthd S cl) (S e2)
||  S (Elet r ds e) = 
	let e' = S e in
	let bdss = sccds ds in
	let (bdss', _) = Umapsnd remds (reverse bdss) (Egetid e') in
	let bdss'' = filter (not o null o snd) (reverse bdss') in
	reduce (\(r, ds).Elet r ds) e' bdss''
||  S (Econstr c el) = Econstr c (map S el)
||  S (Efailmatch n) = Efailmatch n
||  S (e as Ecfunction _ _) = e
||  S (Eidapl i es) = Eidapl i (map S es)
||  S (Elaml is e) = Elaml is (S e)
||  S (Einfo f e) = Einfo f (S e)
||  S _ = fail ("remee "@pr e)
)
end
