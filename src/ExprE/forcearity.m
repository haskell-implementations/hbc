module
#include "../expr/id.t"
#include "../ExprE/Expr_t.t"
#include "../ExprE/Eprint.t"
#include "../expr/einfo_t.t"
#include "../misc/misc.t"
#include "../misc/util.t"
#include "../misc/flags.t"
#include "../simpl/simpl.t"	/* apply */
#include "../transform/hexpr.t"	/* idisfail */
#include "Egetid.t"

#define MoreEta false

export eforcearity, etaexpand;
rec eforcearity (Emodule i e iess) u =
    let (iess', uu) = (Umap (Umap f) iess u
        where f (i, e) u =
	    let (e', u') = farity e u in
	    ((i, e'), u'))
    in (Emodule i e iess', uu)

-- Force the arity if requested
and farity (Einfo (forcearity n) e) u = forcen false n e u
||  farity e u = (e, u)
and forcen b n e u =
    if ~b & n < 0 then fail ("forcen "@itos n@" < 0 : "@pr e) else
	let (e0, fa) = case e in Einfo (forcearity a) e : (e, a) || _ : (e, -1) end in
        let (r as (e, u)) =
        case e0 in
	    Elaml is _ & (length is = n) : (e, u)
	||  Elaml is e' & (length is > n) : if b then (e, u) else (Elaml (head n is) (Elaml (tail n is) e'), u)
	||  Elaml is e & (length is < n) : 
		let nis = map (mknewid "ee") (count u (u+(n-length is)-1)) in
		(Elaml (is@nis) (apply (map (\i.Eidapl i []) nis) e), u+n-length is)
        ||  Einfo inline _ : (e, u)
        ||  e : if n <= 0 then (e, u) else
		let is = map (mknewid "ee") (count u (u+n-1)) in 
		(Elaml is (apply (map (\i.Eidapl i []) is) e), u+n)
        end in
	if fa < 0 then r else (Einfo (forcearity fa) e0, u)

-- We need special eta expansion to ensure that method calls are saturated.
and etaexpand (t as Emodule i e iess) u =
  if EtaExpand | UseForceArity then
    let (iess', (uu,_)) = Umap (Umap expand) iess (u,[])
    in  (Emodule i e iess', uu)
  else
    (t, u)
and type As == List (Id # Int)
and type St == (Int # As)
and expand :: (Id # Expr) -> St -> ((Id#Expr) # St)
and expand (i, e) (ua as (u,a)) =
	let (e', u') = expandinner a e u in
	let ns = if EtaExpand then wantarity [] a e' else [] in
	if null ns | ~allsame ns then
	    ((i, e'), (u', eadd i e' a))
	else
--(if hd ns > 10 then trace ("expand "@itos (hd ns)@" "@prid i@" "@pr e') else (\x.x))
(
	    let (e'', u'') = forcen true (hd ns) e' u'
	    in  ((i, e''), (u'', eadd i e'' a))
)
and eadd i (Einfo (forcearity _) _) a = a
||  eadd i (Elaml is _) a = (i, length is) . a
||  eadd i _            a = (i, 0) . a
and expandinner :: As -> Expr -> Int -> (Expr # Int)
and expandinner a (Ecase e cies de) u =
	let (e', u') = expandinner a e u in
	let (cies', u'') = Umapthd (expandinner a) cies u' in
	let (de', u''') = expandinner a de u''
	in  (Ecase e' cies' de', u''')
||  expandinner a (Elet r ds e) u =
	let (ds', (u',a')) = Umap expand ds (u,a) in
	let (e', u'') = expandinner a' e u'
	in  (Elet r ds' e', u'')
||  expandinner a (Einfo (forcearity k) e) u =
	let (e', u') = expandinner a e u in
        let (e'', u'') = forcen true k e' u'
        in  (Einfo (forcearity k) e'', u'')
||  expandinner a (Eidapl i es) u =
	let (es', u') = Umap (expandinner a) es u
	in  (Eidapl i es', u')
||  expandinner a (Econstr c es) u =
	let (es', u') = Umap (expandinner a) es u
	in  (Econstr c es', u')
||  expandinner a (Elaml is e) u =
	let (e', u') = expandinner a e u
	in  (Elaml is e', u')
||  expandinner a (e as Efailmatch _) u = (e, u)
||  expandinner a (e as Ecfunction _ _) u = (e, u)
||  expandinner a (Einfo i e) u =
	let (e', u') = expandinner a e u
	in  (Einfo i e', u')
||  expandinner a e u = fail ("expandinner: "@pr e)
and wantarity v a (Ecase e cies de) = if MoreEta & null (intereq eqid v (Egetid e)) then concmap (wantarity v a) (de.map thd cies) else [0]
||  wantarity v a (Elet _ ies e)    = if MoreEta & null (intereq eqid v (concmap (Egetid o snd) ies)) then wantarity v a e else [0]
||  wantarity v a (Econstr _ _) = [0]
||  wantarity v a (Ecfunction _ _) = [0]
||  wantarity v a (Eidapl i [_]) & (isidfail i) = []	-- Pfail ignores extra args
||  wantarity v a (e as Eidapl i es) = 
	let a = assocdefeq eqid i a (arity_of_id i)
	and n = length es in
	if a = -1 then
	    [0]		-- we don't know, so be careful
	else
--if a-n < 0 then trace ("a="@itos a@", n="@itos n@", e="@pr e) [a-n] else
	    [a-n]
||  wantarity v a (Elaml is e) = map (+ length is) (wantarity (is@v) a e)
||  wantarity v a (Efailmatch _) = []			-- don't care
||  wantarity v a (Einfo metcall e) = []		-- WHY??
--||  wantarity v a (Einfo (forcearity n) _) = [n]
||  wantarity v a (Einfo _ e) = wantarity v a e
||  wantarity v a e = fail ("wantarity: "@pr e)
end
