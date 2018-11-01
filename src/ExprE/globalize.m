module
#include "../misc/misc.t"
#include "../misc/util.t"
#include "../misc/flags.t"
#include "../expr/einfo_t.t"
#include "../expr/id_t.t"
#include "../expr/id.t"
#include "Expr_t.t"
#include "Eprint.t"
export globalize;
-- try to pull out constant expressions, improves lambda lifting later.
rec globalize (e as Emodule i ex dl) =
    if Globalize then
	Emodule i ex (fst (Umap (Uconcmap glob) dl []))
    else
	e
and glob (i,e) u = 
    let (ies, e') = globe u e in
    let ies' = ies@[(i,e')]
    in  (ies', u@bgids ies')
and globe u (Eap f a) =
    let (ies, f') = globe u f
    and (ies', a') = globe u a
    in  (ies@ies', Eap f' a')
||  globe u (Elam i e) =
    let (ies, e') = globe u e
    in  (ies, Elam i e')
||  globe u (Ecase e cl de) =
    let (ies, e') = globe u e
    and (iess, es) = split (map (globe u o thd) cl)
    and (ies', de') = globe u de
    in  (ies@conc iess@ies', Ecase e' (map2 (\ (c,is,_).\e.(c,is,e)) cl es) de')
||  globe u (Elet r ies e) =
    let doit u' = 
	let (gg as (gb, ngb)) = partition (isglob (u'@u)) ies
        in  bgids gb
    in
    let rec again is =
        let is' = doit is in
        if length is = length is' then is else again is'
    in
    let u' = again (bgids ies) @ u in
    let (ies', e') = globe u' e
    and (gb, ngb) = partition (isglob u') ies in
    (gb@ies', elet r ngb e')
||  globe u (Einfo t e) =
    let (ies, e') = globe u e
    in  (ies, Einfo t e')
||  globe u (Econstr c es) =
    let (iess, es') = split (map (globe u) es)
    in  (conc iess, Econstr c es')
||  globe u e = ([], e)
and isglob u (i, e) = all (\i.id_is_visible i | member eqid i u) (getids e)
||  isglob _ _ = false
and bgids ies = map fst ies
and gids (Eap f a) = gids f @ gids a
 || gids (Elam i e) = filter (\i'.~ eqid i i') (gids e)
 || gids (Ecase e pes de) = 
         gids e @ concmap (\ (_,is,e).diffeq eqid (gids e) is) pes @ gids de
 || gids (Evar i) & (isdummy i) = []
 || gids (Evar i) = [i]
 || gids (Econstr _ el) = concmap gids el
 || gids (Einfo t e) = gids e
 || gids (Elet _ ies e) = diffeq eqid (gids e @ concmap (gids o snd) ies) (bgids ies)
 || gids _ = []
and
    getids e = mkseteq eqid (gids e)
and elet _ [] e = e
||  elet r ds e = Elet r ds e
end
