module
#include "../expr/id_t.t"
#include "../expr/constr_t.t"
#include "../expr/constrfun.t"
#include "../expr/booltree.t"
#include "../ExprE/Expr_t.t"
#include "../expr/einfo_t.t"
#include "../expr/einfo.t"
#include "../expr/id.t"
#include "../expr/impexp_t.t"
#include "../misc/setofid.t"
#include "../misc/misc.t"
#include "../misc/flags.t"
#include "../funnos.h"
#include "../ExprE/Eprint.t"
#include "../transform/hexpr.t"
#include "../misc/util.t"
#include "usage.t"
export strictanal, dostrictanal, sidinfo, smkinfotab, strictconvar;
rec
    thd (_,_,x) = x
and impinfo (i, _, f) = (id_no i,f)
and isfail (Efailmatch 0) = true
||  isfail (Eidapl i _) & (isidfail i) = true
||  isfail _ = false
and Isl [] = []
||  Isl (a.b) = reduce Is a b
and iscanon (Einfo strict _) = true
||  iscanon (Einfo noeval _) = true
||  iscanon (Econstr c _) = not (exists snd (cargs c))
||  iscanon _ = false
and not0 (Econstr c _) = cno c ~= 0
||  not0 _ = false
and mkstrict (e as Einfo strict _) = e
||  mkstrict (e as Econstr _ _) = e
||  mkstrict (e as Einfo noeval _) = e
||  mkstrict (e as Eidapl _ []) = e    -- Somewhat dubious?
||  mkstrict e = Einfo strict e
and strictvar a e = (svar e
    where rec
    svar (Ecase e cies ed) =
    	if isfail ed then
	    Iu (svar e) (Isl (map (svar o thd) cies))
	else
	    Iu (svar e) (Is (svar ed) (Isl (map (svar o thd) cies)))
||  svar (Elet r ies e) =
	let s = svar e in
	if r then s
	else reduce (\(i,e).\r.if Imem i s then Iu (svar e) r else r) s ies
||  svar (Eidapl i []) = [i]
||  svar (Eidapl i es) =
	case sidinfo a i in
	    finfo n _ (btands u,_) _ _ :
                let ss = map (\n.mem (btors[btvar n]) u) (count 0 (n-1)) in
		if length es < n then []
		else reduce (\(s,e).\r.if s | strannot e then Iu (svar e) r else r) []
			    (combine (ss, head n es))
	end
||  svar (Einfo _ e) = svar e
||  svar _ = [])

and strannot (Einfo strict _) = true
||  strannot _ = false

and sidinfo a (mkid _ _ (idi_var (var_global f) _ _) (Orignames Vimported _ _)) = f
||  sidinfo a (mkid m _ (idi_var (var_pre f) _ _) _) = f
||  sidinfo a (mkid m _ _ _) = assocdef m a f_unk

and strictconvar c is =
-- This does not work in general, just take a look at Word.hs and x `bitAnd` y == z
--let rrr =
	if nconstrs c = 1 & length is = 1 then
	    []		-- possible isomorphic type dcl, strictness does not "count"
	else
	    conc (map2 (\ (_, b).\i.if b then [i] else []) (cargs c) is)
--in trace ("strictconvar "@pr (Econstr c is)@show_list prid rrr) rrr

and iscaseok (Einfo noeval _) = true
||  iscaseok (Einfo strict _) = true
||  iscaseok (Einfo _ e) = iscaseok e
||  iscaseok (Econstr _ _) = true
||  iscaseok (Efailmatch n) = n = 0
||  iscaseok (Elet _ _ e) = iscaseok e
||  iscaseok _ = false

and sanal eis a sis vs e = /*trace ("sanal "@show_list prid vs@" "@pr e)*/ (sa vs e
where rec
    sa vs (Ecase e cies ed) =
	let (vs1, ne) = sa vs e in
	let vsn = Iu vs1 vs in
--	let (vss, nes) = split (map (sa vsn o thd) cies)
-- A hack to get more strict variables.  REDO ALL OF THIS!!
	let (vss, nes) = split (map (\(c,is,e).sanal eis a (Iu (strictvar a e) sis) (Iu vsn (strictconvar c is)) e) cies)
	and (vsd, ned) = sanal eis a (Iu (strictvar a ed) sis) vsn ed in
	let vs2 = if isfail ed then Isl vss else Is vsd (Isl vss)
	and ncies = map2 (\(c,is,_).\e.(c,is,e)) cies nes in
	let ec = Ecase ne ncies ned in
	(Iu vs1 vs2, if all iscaseok (ne.ned.map thd ncies) then mkstrict ec else ec)
||  sa vs (Elet r ies e) = 
	let (nvss, nds) = split (map f ies)
	where f (i,e) =
		let (nvs, ne) = sa vs e in
		if r then
			([], (i, ne))
		else if Imem i sis then
			(Iu [i] nvs, (i, mkstrict ne))
		else
			((if iscanon ne then [i] else []), (i, ne))
	in
	let vs1 = reduce Iu [] nvss in
	let (vs2, ne) = sa (Iu vs1 vs) e in
	(Iu vs1 vs2, Elet r nds ne)
||  sa vs (Eidapl i []) =
	if EvalOpt & Imem i vs & ~ (nuflag & id_is_global i) then 
	    ([], Einfo noeval (Eidapl i []))
	else
	    ([i], Eidapl i [])
-- A hack to handle left-to-right behaviour of dyadic predefined functions
||  sa vs (Eidapl i [e1;e2]) & (id_is_predef i & ~ specialpre i) =
        let m = id_no i in
	let (nvs1, ne1) = sa vs e1 in
	let (nvs2, ne2) = sa (Iu nvs1 vs) e2 in
	if iscanon ne1 & iscanon ne2 & (m ~= Fdiv & m ~= Fmod | not0 ne2) then
		(Iu nvs2 (Iu nvs1 vs), mkstrict (Eidapl i [ne1; ne2]))
	else
		(Iu nvs2 (Iu nvs1 vs), Eidapl i [ne1; ne2])
||  sa vs (Eidapl i es) =
	case sidinfo a i in
	    finfo n _ (btands u,w) _ _ :
-- strictness info could be used better!
		let (s, ts) = termi n w in
                let ss = map (\n.mem (btors[btvar n]) u) (count 0 (n-1)) in
		let mustev = assocdefeq eqid i eis falses in
		if length es < n then
			([], Eidapl i (map (snd o sa vs) es))
		else
			let (nvss, nes0) = split (map (sa vs) es) in
			let nes = map2 (\e.\b.if b then mkstrict e else e) nes0 mustev in
(if ~ (Or (head 10 mustev) & X4) then (\x.x) else trace ("mustev "@show_list show_bool mustev@" "@pr (Eidapl i nes0)@pr (Eidapl i nes)))
(
			let xx = reduce (\(s,is).\r.if s then Iu is r else r)
					[] (combine (ss, head n nvss)) in
			if TermCall & length es = n & s & w ~= btff & And (map2 (\ne.\t.~t | iscanon ne) nes ts) then
				(xx, mkstrict (Eidapl i nes))
			else
				(xx, Eidapl i nes)
)
	end
||  sa vs (Econstr c es) =
	([], Econstr c (map (snd o sa vs) es))
||  sa vs (Efailmatch n) = ([], Efailmatch n)
||  sa vs (e as Ecfunction _ _) = ([], e)
||  sa vs (Einfo i e) = let (vs, e1) = sa vs e in (vs, Einfo i e1)
||  sa vs _ = fail "sa"
)

and falses = false . falses

and termi m (btands xs) = (true, map (\n.mem (btors[btvar n]) xs) (count 0 (m-1)))
||  termi m _ = (false, [])

and sc a (Ecase e cies ed) = Ecase (sc a e) (mapthd (sc a) cies) (sc a ed)
||  sc a (Elet r ies e) = Elet r ies (sc a e)
||  sc a (e as Eidapl _ []) = e
||  sc a (e as Eidapl i es) =
	case sidinfo a i in
	    finfo n _ (btands u,_) _  _:
		if length es ~= n then e else
                let ss = map (\n.mem (btors[btvar n]) u) (count 0 (n-1)) in
		Eidapl i (map2 (\e.\b.if b then amkstrict a (sc a e) else e) es ss)
	end
||  sc a (e as Econstr _ _) = e
||  sc a (e as Efailmatch _) = e
||  sc a (Einfo i e) = Einfo i (sc a e)
||  sc a e = fail ("strict-sc "@pr e)

-- Don't make unsaturated calls strict.
and amkstrict a (e as Eidapl i es) & 
	((StrictCall | id_is_predef i) &
	 let (finfo n _ _ _ _) = sidinfo a i in length es = n) = mkstrict e
||  amkstrict a e = e

and si eis a vs is e = 
	let sis = strictvar a e in
        let e' = snd (sanal eis a sis vs e) in
	sc a e'

and evids eis i is = Imk (compr (assocdefeq eqid i eis []) is)

and tr a eis (i, (Elaml is e)) =
	(updfinfo i (assocdef (id_no i) a f_unk), Elaml is (si eis a (evids eis i is) is e))
||  tr a eis (i, e) = 
	(updfinfo i (assocdef (id_no i) a f_unk), si eis a [] [] e)

and updfinfo (mkid n s (idi_var _ t osel) on) f = mkid n s (idi_var (var_global f) t osel) on
||  updfinfo i f = i

and mkfinfo (-1,_,_) = f_unk
||  mkfinfo (a, s, t) = finfo a [] (s, t) (-1) None

and smkinfotab sinf = map (\(i,p).(id_no i, mkfinfo p)) sinf

and dostrictanal (Emodule i es bs) sinf einf =
        let ifs = smkinfotab sinf in
	let nbs = map (map (tr ifs einf)) bs in
	let f (mkexpid i) & (id_visibility i ~= Vimported) =
	         mkexpid (updfinfo i (assocdef (id_no i) ifs f_unk))
	 || f i = i in
let r =
	Emodule i (map f es) nbs
in
    if X4 then
	trace ("dostructanal "@show_list (show_pair(prid,show_list show_bool)) einf) r
    else
	r
and strictanal e sinf =
        let e' = dostrictanal e sinf [] in
	if UsageAnal then
	    usageanal e e' sinf
	else
	    e'
end
