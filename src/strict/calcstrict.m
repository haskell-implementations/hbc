module
#include "../expr/id_t.t"
#include "../expr/id.t"
#include "../expr/einfo_t.t"
#include "../expr/booltree.t"
#include "../expr/constrfun.t"
#include "../ExprE/Expr_t.t"
#include "../ExprE/Eprint.t"
#include "../ExprE/Egetid.t"
#include "../misc/util.t"
#include "../misc/flags.t"
#include "../transform/hexpr.t"
#include "strict.t"		/* strictconvar */
export calcstrict, prstrict;
rec 

    calcstrict (Emodule _ _ dss) = reduce recfuns [] (reverse dss)

and prstrict = show_list (show_pair(prid, (\(a,s,t)."("@itos a@")  "@show_BT s@" # "@show_BT t)))

and recfuns ds s =
--trace ("recfuns "@show_list (\ (i,_).prid i@":"@itos (id_no i)) ds)
(
    let cis = map fst ds in
    if all islamapl ds then
	manyfuns cis ds s
    else
	reduce (onedef cis) s ds	-- could be better for mutually recursive non-functions
)
and islamapl (_,Elaml _ _) = true
||  islamapl _ = false

and onedef cis (d as (i, Elaml _ _)) s = manyfuns cis [d] s
||  onedef cis (i, e) s = 
    let (bt, s') = S [(i, 0)] [] s e in
    let b = btsimpl bt in
    let [r] = findfix [(id_no i,btff)] [(id_no i,b)] in
    (i, (0, r, term cis [] s e)) . s' @ s

and manyfuns cis ds s =
    let ias = mapsnd (\(Elaml is _).length is) ds in
    let (bts, ss) = split (map (\(i,Elaml is e).absfun ias s is e) ds) in
    let bs = map btsimpl bts in
    let rs = findfix (combine (map (id_no o fst) ds,bs)) (map (\ (i,_).id_no i, btff) ds) in
    map2 (\(i,Elaml is e).\r.(i, (length is, r, term cis (combine(is,map btvar (from 0))) s e))) ds rs @ conc ss @ s

and absfun ias s is e = 
--let rrr =
    S ias (combine(is,map btvar (from 0))) s e
--in trace ("absfun "@pr (Elaml is e)@"=\n++++++ "@show_BT (fst rrr)@"\n++++++ "@show_BT (btsimpl (fst rrr))) rrr

and findfix bs cur =
/*
trace (force (
     "findfix bs  = "@show_list (show_pair(show_int,show_BT)) bs @"\n"@
     "+++++++ cur = "@show_list (show_pair(show_int,show_BT)) cur
))
*/
(
    let cur' = map (\ (f, bt).(f, btsubfun cur bt)) bs in
    if cur' = cur then
	map snd cur
    else
	findfix bs cur'
)

-- should be redone by having a subst operation in booltree.
and btsubfun cur bt = btsimpl (btsubfun' cur bt)
and btsubfun' cur (btands xs) = btands (map (btsubfun' cur) xs)
||  btsubfun' cur (btors xs)  = btors  (map (btsubfun' cur) xs)
||  btsubfun' cur (bt as btvar _) = bt
||  btsubfun' cur (btapp f bts) = substbt (map (btsubfun' cur) bts) (assocdef f cur (fail "btsubfun assoc"))

and defstr r s ds = recfuns ds s   --reduce onedef s ds

and S c r s (Econstr _ _) = (bttt, [])
||  S c r s (Ecfunction _ _) = (bttt, [])
||  S c r s (Elet false ds e) /*& (~exists islamapl ds)*/ = 
    let (bts, sss) = split (map (\(_,e).S c r s e) ds) in
    let (bt, ss) = S c (combine(map fst ds, bts) @ r) s e in
    (bt, conc (ss.sss))
||  S c r s (Elet _ ds e) = 
    let s' = defstr r s ds in
    let (bt, ss) = S c r s' e in
    (bt, head (length s' - length s) s' @ ss)
||  S c r s (e as Ecase e1 cs e2) = 
    let (bt1, s1) = S c r s e1
    and (bts, ss) = split (map (\(_,_,e).S c r s e) cs)
    and (bt2, s2) = S c r s e2 in
    (btands [bt1; btors (bt2.bts)], conc (s1.s2.ss))
||  S c r s (Efailmatch _) = (btff, [])
||  S c r s (Eidapl i []) = (assocdefeq eqid i r bttt, [])
||  S c r s (Eidapl i _) & (FailStrict & isidfail i) = (btff, [])
||  S c r s (Eidapl i es) = 
    if assocdefeq eqid i c (-1) = length es then
	let (bts, ss) = split (map (S c r s) es) in
	(btapp (id_no i) (map btsimpl bts), conc ss)
    else case assocdefeq eqid i s (globfind i) in
            (a, bt, _) & (a = length es) : 
		 let (bts, ss) = split (map (S c r s) es) in
--trace ("S "@show_BT bt@" --- "@show_BT (substbt bts bt)@" --- "@show_BT (btsimpl (substbt bts bt)))
		 (btsimpl (substbt bts bt), conc ss)
         || _ : (bttt, [])
         end
||  S c r s (Einfo f e) = S c r s e
||  S c r s e = fail ("calcstrict-S "@pr e)

#if 0
#if 1
and term _ _ _ _ = btff
#else
and term is r s e = T is r s e
and T cis r s (Econstr c es) =
	let tbs = cargs c in
	if exists snd tbs | ~And (map (\e.T cis r s e = bttt)) then
	    btff
	else
	    bttt
||  T cis r s (Eidapl i []) = ??
||  T cis r s _ = btff
#endif

#else
--Something wrong with ../htest/loop.m

-- Check if an expression is guaranteed to terminate.
and term cis r s e =
    if ~null (intereq eqid cis (Egetid e)) then
	btff		-- any kind of recursion is dangerous
    else
	btsimpl (T r s e)
and T :: List (Id#BT) -> List (Id#(Int#BT#BT)) -> Expr -> BT
and T r s (ec as Econstr c es) = 
let xs = (map2 (\e.\ (_,b).if b then T r s e else bttt) es (cargs c))
in
--trace (pr ec@show_list show_BT xs@show_list (show_bool o snd) (cargs c))
(
	btands xs
)
||  T r s (Elet false ds e) /*& (~exists islamapl ds)*/ = 
    let bts = map (\(_,e).T r s e) ds in
    T (combine(map fst ds, bts) @ r) s e
||  T r s (Elet true _ _) = btff
||  T r s (Ecase e1 cs e2) = 
    let bt1 = T r s e1
    and bts = map (\(c,is,e).T (map (,bttt) (strictconvar c is) @ r) s e) cs
    and bt2 = T r s e2 in
    btands (bt1.bt2.bts)
||  T r s (Efailmatch _) = bttt
||  T r s (Eidapl i []) = assocdefeq eqid i r btff
--||  T r s (Eidapl i _) & (FailStrict & isidfail i) = bttt
||  T r s (Eidapl i es) = 
         case assocdefeq eqid i s (globfind i) in
            (a, _, bt) & (a = length es) : 
--let rrr =
		 let bts = map (T r s) es in
		 btsimpl (substbt bts bt)
--in trace ("T: "@pr (Eidapl i es)@" "@show_BT bt@" "@show_BT rrr) rrr
         || _ : btff
         end
||  T r s (Einfo f e) = T r s e
||  T r s e = fail ("calcstrict-T "@pr e)
#endif

#if 0
-- Be very careful with constructors. Saying that f terminates may cause a recursive call to f inside a constructor.
and termce (Econstr c es) = ~exists snd (cargs c) & all termce es
||  termce (Eidapl _ []) = true
||  termce _ = false
#endif

-- Eap, Elam, Evar, Emodule cannor occur 
and globfind (mkid _ _ (idi_var (var_global (finfo a _ (s,t) _ _)) _ _) _) = (a, s, t)
||  globfind (mkid _ _ (idi_var (var_pre    (finfo a _ (s,t) _ _)) _ _) _) = (a, s, t)
||  globfind _ = (-1, bttt, btff)
end
