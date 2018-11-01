module
#include "../expr/id_t.t"
#include "../expr/id.t"
#include "../expr/einfo_t.t"
#include "../expr/impexp.t"
#include "../ExprE/Expr_t.t"
#include "../misc/misc.t"
#include "../expr/booltree.t"
export addarity;
rec
    mkfinfo n k = finfo n [] (bttt,btff) k None		-- fake some parts
and findfs _ [] = -1
||  findfs i ((i',s).iss) = if eqid i i' then s else findfs i iss
and getes fs ((i as mkid n s _ on), Elaml is _) = (n, mkid n s (idi_var (var_global (mkfinfo (length is) (findfs i fs))) Onotype None) on)
||  getes fs ((i as mkid n s _ on), _) =          (n, mkid n s (idi_var (var_global (mkfinfo 0           (findfs i fs))) Onotype None) on)
and addarity (Emodule i exps dss) fs =
	(Emodule i exps (snd (mapstate addd [] dss))
	    where addd tab ds =
	let etab = map (getes fs) ds in
	let atab = etab@tab in
	let find (i as mkid n _ (idi_var _ _ _) _) = assocdef n atab i 
         || find i = i in
	((atab, map (\(i,e).(find i, aa e)) ds))

where rec
    aa (Ecase e ps ed) = Ecase (aa e) (mapthd aa ps) (aa ed)
||  aa (e as Efailmatch _) = e
||  aa (Elet re ds e) = Elet re (mapsnd aa ds) (aa e)
||  aa (Econstr c es) = Econstr c (map aa es)
||  aa (Eidapl i es) = Eidapl (find i) (map aa es)
||  aa (Einfo (doeval is) e) = Einfo (doeval (map find is)) (aa e)
||  aa (Einfo i e) = Einfo i (aa e)
||  aa (Elaml is e) = Elaml is (aa e)
||  aa (e as Ecfunction _ _) = e
)
end
