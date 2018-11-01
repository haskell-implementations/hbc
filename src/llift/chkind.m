module
#include "../expr/id_t.t"
#include "../expr/id.t"
#include "../expr/einfo_t.t"
#include "../ExprE/Expr_t.t"
#include "../misc/misc.t"
#include "../misc/setofid.t"
export achkind;
rec
    achkind (Emodule imps exps ds) =
	Emodule imps exps (map (map LR) ds)
where rec
    LR (i, Elaml is e) = (i, Elaml is (R [] e))
and -- Extract evaluated variables
    Rv (Ecase e ps ed) = Iu (Rv e) (reduce Is [] (Rv ed.map (\(_,_,e).Rv e) ps))
||  Rv (e as Efailmatch _) = []
||  Rv (Elet re ds e) =
	if re then
		Rv e
	else
		reduce Iu [] (Rv e.map Lv ds)
||  Rv (Econstr c es) = []
||  Rv (Ecfunction _ _) = []
||  Rv (Einfo noeval (Eidapl i [])) = []
||  Rv (Eidapl i es) = [i]
||  Rv (Einfo (doeval is) e) = Iu (Imk is) (Rv e)
||  Rv (Einfo i e) = Rv e
and -- Evaluated vars from a let binding
    Lv (i, Einfo strict e) = Iu [i] (Rv e)
||  Lv _ = []
and -- Convert evaled to chkind
    R t (Ecase e ps ed) = let t' = Iu t (Rv e) in Ecase (R t e) (mapthd (R t') ps) (R t' ed)
||  R t (e as Efailmatch _) = e
||  R t (Elet re ds e) = 
	if re then
		Elet re (mapsnd (R t) ds) (R t e)
	else
		let t' = reduce Iu [] (t.map Lv ds) in
		Elet re (mapsnd (R t) ds) (R t' e)
||  R t (Econstr c es) = Econstr c (map (R t) es)
||  R t (e as Einfo noeval (Eidapl i [])) = if /*id_is_global i |*/ Imem i t then e else Einfo chkind (Eidapl i [])
||  R t (Eidapl i es) = Eidapl i (map (R t) es)
||  R t (Einfo (doeval is) e) = Einfo (doeval is) (R (Iu t (Imk is)) e)
||  R t (Einfo i e) = Einfo i (R t e)
||  R t (e as Ecfunction _ _) = e
end
