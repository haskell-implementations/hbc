module -- addspec
#include "../expr/id_t.t"
#include "../expr/id.t"
#include "../expr/einfo_t.t"
#include "../expr/pragma_t.t"
#include "../expr/ttype_t.t"
#include "../expr/ttype.t"
#include "../misc/misc.t"
#include "../misc/util.t"
#include "../misc/sort.t"
#include "../misc/flags.t"
#include "../rename/renameutil.t"	/* specname */
#include "../type/subst.t"		/* TRtype */
#include "Egetid.t"
#include "Esubst.t"
#include "Expr_t.t"
#include "Eprint.t"
export addspec, collectspec, addspecrestr, addautospec;
rec type Pragmas == List (Id#(List Pragma))
and type Def == Id # Expr
and type Defs == List Def
and type ITs == List (Id # (List (Id#(Ttype # (Option Id)))))
and type Inner == ITs -> Expr -> Int -> (Expr#Int)
and addspec ps e u = 
	if Specialize then 
	    addsp ( \_.\x.\u.(x,u)) (mapsnd (concmap getspect) ps) e u 
	else 
	    (e, u)
and addsp :: Inner -> ITs -> Expr -> Int -> (Expr#Int)
and addsp f [] e u = (e, u)
||  addsp f ps (Emodule is es dss) u = 
	let (dss, u') = Umap (addspecds f ps) dss u in
	(Emodule is es dss, u')
and addspecds :: Inner -> ITs -> Defs -> Int -> (Defs # Int)
and addspecds f ps ds u = 
	let (dds, u') = Umap (addspecd f ps) ds u in
	(conc dds, u')
and addspecd :: Inner -> ITs -> Def -> Int -> (Defs # Int)
and addspecd g ps (i, e) u =
	let ts = assocdefeq eqid i ps [] in
	let (e0, u0) = g ps e u in
	let (ds, u') = Umap (addspece i e0) ts u0 in
--trace ("addspecd "@prid i@" "@show_list (show_pair(prid,pr)) ds@"\ne="@pr e@"\ne0="@pr e0)
	((i, e0).ds, u')
#if 0
	if ~id_is_visible i & length ts = 1 then
	    -- common case
	    ([(i, Einfo (restr _ (snd (hd ts))) e)], u0)
	else
	    let (ds, u') = Umap (addspece i e0) ts u0 in
	    let ds' = if id_is_visible i then d.ds else ds in
	    (ds', u')
#endif
and addspece :: Id -> Expr -> (Id # (Ttype # (Option Id))) -> Int -> ((Id#Expr) # Int)
and addspece i e (it as (i', (t, None))) u = 
	let (e', u') = alphacopy e u in
	((i', Erestr t e'), u')
||  addspece i e (it as (i', (t, Some ii))) u = ((i', Erestr t (Evar ii)), u)
and alphacopy e u = 
	let is = Egetlamid e in
	let addspecf (i as mkid _ a b on) n = (i, mkid n a b on) in
        let iis = map2 addspecf is (from u) in
	renumbertrestr (Ealphasubst iis e) (u+length is)
and getspect (Pspecialize i [t]) = [(i, t)]
||  getspect _ = []
and Erestr t (Einfo (restr _ _) e) = Erestr t e
||  Erestr t e = Einfo (restr [] t) e

and gtype S tv =
--trace (prTR S)
(
    let t = TRtype S tv in
    let vs = getTvars t in
    normtype (xmkcontype (extractcon vs S) t)
)

and mknewid i t u =
	case i in
	   mkid _ s info (Orignames vis fix (mi,_)) : let s' = specname s t in (mkid u s' info (Orignames vis fix (mi,s')), t)
	|| mkid _ s info Noorigname                 : let s' = specname s t in(mkid u s' info Noorigname                 , t)
	end
and collectspec S0 (Emodule _ _ dss) u =
        let S = usekall S0 in
	let its = concmap (concmap collectd) dss in
	let itss = groupsort (\ (i1,_).\ (i2,_).ltid i1 i2) its in
	let f (its as ((i,_)._)) u =
		let ts = mkseteq eqtype (map (gtype S o snd) its) in
		let ts = filter (null o cpart) ts in -- for the moment we cannot handle spec. with context
		((i, map2 (mknewid i) ts (from u)), u+length ts)
	in  let (its, u) = Umap f itss u in
	    (filter (not o null o snd) its, u)
and collectd (i, e) = collecte e
and collecte (Ecase e pl d) = collecte e @ conc (map (collecte o thd) pl) @ collecte d
||  collecte (Elet _ ds e) = conc (map (collecte o snd) ds) @ collecte e
||  collecte (Econstr _ es) = concmap collecte es
||  collecte (Efailmatch _) = []
||  collecte (Ecfunction _ _) = []
||  collecte (Einfo (srestr t) e) = [(Eleft e, t)]
||  collecte (Einfo _ e) = collecte e
||  collecte (Evar _) = []
||  collecte (Eap f a) = collecte f @ collecte a
||  collecte (Elam _ e) = collecte e
||  collecte e = fail ("collecte "@pr e)
and Eleft (Evar i) = i
||  Eleft (Eap f _) = Eleft f
||  Eleft (Einfo _ e) = Eleft e
||  Eleft e = fail ("Eleft "@pr e)

-- Don't do specialization of some things.
and oktospec i = 
    case idtostr i in
       'M'._ : false
    || 'D'._ : false
    || 'V'._ : false
    || _     : true
    end

and addspecrestr (e as Emodule is es dss) u =
    if AutoSpecialize then
	let rec f is [] r u = (reverse r, u)
	     || f is (ds.dss) r u =
		let is' = filter oktospec (map fst ds) @ is in
		let (ds', u') = Umapsnd (S is') ds u in
		f is' dss (ds'.r) u'
	in  Uap (Emodule is es) (f [] dss [] u)
    else
	(e, u)
and S b (Elam i e) u = Uap (Elam i) (S b e u)
||  S b (Eap e1 e2) u =
	let (e1', u') = S b e1 u in
	Uap (Eap e1') (S b e2 u')
||  S b (e as Evar i) u =
	if AutoSpecialize & member eqid i b then
	    (Einfo (srestr (mktvar u)) e, u+1)
	else
	    (e, u)
||  S b (Ecase e pl dp) u =
	let (e', u') = S b e u in
	let (pl', u'') = Umapthd (S b) pl u' in
	let (dp', u''') = S b dp u'' in
   	(Ecase e' pl' dp', u''')
||  S b (Elet r dl e) u =
	let b' = map fst dl@b in
	let (dl', u') = Umapsnd (S b') dl u in
	Uap (Elet r dl') (S b' e u')
||  S b (Econstr c el) u = Uap (Econstr c) (Umap (S b) el u)
||  S b (e as Efailmatch n) u = (e, u)
||  S b (e as Ecfunction _ _) u = (e, u)
||  S b (e as (Einfo notchk) _) u = (e, u)
||  S b (Einfo f e) u = Uap (Einfo f) (S b e u)
||  S b e u = fail ("addspecrestr "@pr e)

and innerspec ps (Elam i e) u = Uap (Elam i) (innerspec ps e u)
||  innerspec ps (Eap e1 e2) u =
	let (e1', u') = innerspec ps e1 u in
	Uap (Eap e1') (innerspec ps e2 u')
||  innerspec ps (e as Evar _) u = (e, u)
||  innerspec ps (Ecase e pl dp) u =
	let (e', u') = innerspec ps e u in
	let (pl', u'') = Umapthd (innerspec ps) pl u' in
	let (dp', u''') = innerspec ps dp u'' in
   	(Ecase e' pl' dp', u''')
||  innerspec ps (Elet r ds e) u =
	let (ds', u') = addspecds innerspec ps ds u in
	let (e', u'') = innerspec ps e u' in
--trace ("innerspec ds="@show_list (show_pair(prid,pr)) ds@"\nds'="@show_list (show_pair(prid,pr)) ds')
(
	if r then
	    (Elet r ds' e', u'')
	else
	    (reduce (\d.\e.Elet r [d] e) e' ds', u'')
)
||  innerspec ps (Econstr c el) u = Uap (Econstr c) (Umap (innerspec ps) el u)
||  innerspec ps (e as Efailmatch n) u = (e, u)
||  innerspec ps (e as Ecfunction _ _) u = (e, u)
||  innerspec ps (e as (Einfo notchk) _) u = (e, u)
||  innerspec ps (Einfo f e) u = Uap (Einfo f) (innerspec ps e u)
||  innerspec ps e u = fail ("innerspec "@pr e)
and addautospec :: (List (Id # (List (Id#Ttype))))->Expr->Int->(Expr # Int)
and addautospec ps e u = if Specialize then addsp innerspec (mapsnd (mapsnd (\t.(t,None))) ps) e u else (e, u)

and doinfo :: (Teinfo->Expr->*a->Expr # *a) -> Expr -> *a -> (Expr # *a)
and doinfo f (Emodule i e dss) u =
	Uap (Emodule i e) (Umap (Umapsnd (doinfo f)) dss u)
||  doinfo f (Elam i e) u = Uap (Elam i) (doinfo f e u)
||  doinfo f (Eap e1 e2) u =
	let (e1', u') = doinfo f e1 u in
	Uap (Eap e1') (doinfo f e2 u')
||  doinfo f (e as Evar _) u = (e, u)
||  doinfo f (Ecase e pl dp) u =
	let (e', u') = doinfo f e u in
	let (pl', u'') = Umapthd (doinfo f) pl u' in
	let (dp', u''') = doinfo f dp u'' in
   	(Ecase e' pl' dp', u''')
||  doinfo f (Elet r ds e) u =
	let (ds', u') = Umapsnd (doinfo f) ds u in
	let (e', u'') = doinfo f e u' in
	(Elet r ds' e', u'')
||  doinfo f (Econstr c el) u = Uap (Econstr c) (Umap (doinfo f) el u)
||  doinfo f (e as Efailmatch n) u = (e, u)
||  doinfo f (e as Ecfunction _ _) u = (e, u)
||  doinfo f (Einfo x e) u = 
	let (e', u') = doinfo f e u in
	f x e' u'
||  doinfo f e u = fail ("doinfo "@pr e)
-- renumber trestr and remove restr
and renumbertrestr e u = (doinfo g e u
	where g (trestr _) e u = (Einfo (trestr (mktvar u)) e, u+1)
	   || g (srestr _) e u = (Einfo (srestr (mktvar u)) e, u+1)
	   || g (restr _ _)  e u = (e, u)
	   || g x          e u = (Einfo x e, u))
end
