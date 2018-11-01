module -- eqtrans
#include "../expr/id.t"
#include "Expr_t.t"
#include "Egetid.t"
#include "../misc/misc.t"
#include "../misc/util.t"

export unrec, sccds;
rec
    sccds ds =
	let ns = map fst ds in
	let ness = map (\(i,e).(i, intereq eqid (mkseteq eqid (Egetid e)) ns)) ds in
	(map f (scceq eqid ness)
		where f [(n, es)] = (member eqid n es, [(n, ass n ds)])
		   || f l = (true, map (\(n, _).(n, ass n ds)) l))
and 
    ass n ds = assocdefeq eqid n ds (fail "assoc in unrec")
and
    urec ds e =	reduce (\(r, ds).Elet r ds) e (sccds ds)
and
    unrec (Eap f a) = Eap (unrec f) (unrec a)
||  unrec (Elam i e) = Elam i (unrec e)
||  unrec (Ecase e pl dp) = 
   		Ecase (unrec e) (mapthd unrec pl) (unrec dp)
||  unrec (Elet false dl e) = Elet false (mapsnd unrec dl) (unrec e)
||  unrec (Elet true dl e) = urec (mapsnd unrec dl) (unrec e)
||  unrec (e as Evar _) = e
||  unrec (Econstr c el) = Econstr c (map unrec el)
||  unrec (Emodule i expl dl) = Emodule i expl (map snd (sccds (mapsnd unrec (conc dl))))
||  unrec (e as Efailmatch _) = e
||  unrec (e as Ecfunction _ _) = e
||  unrec (Eidapl i el) = Eidapl i (map unrec el)
||  unrec (Elaml il e) = Elaml il (unrec e)
||  unrec (Einfo f e) = Einfo f (unrec e)
||  unrec _ = fail "unrec"
end
