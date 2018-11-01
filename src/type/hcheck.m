module -- check
--
-- performs the actual type checking
--
-- The strictification (with let!) is a very small improvement,
-- but since I put it in it can stay...
--
#include "../expr/id_t.t"
#include "../expr/constr_t.t"
#include "../expr/einfo_t.t"
#include "../expr/ttype_t.t"
#include "../expr/ttype.t"
#include "../expr/tinfo.t"
#include "../expr/id.t"
#include "../expr/idtab.t"
#include "../expr/constrfun.t"
#include "../expr/pragma_t.t"
#include "../ExprE/Expr_t.t"
#include "../ExprE/Eprint.t"
#include "../ExprE/Egetid.t"
#include "../ExprE/unrec.t"
#include "../transform/misc.t"
#include "../transform/hexpr.t"	/* numClass */
#include "../misc/misc.t"
#include "../misc/util.t"
#include "../misc/flags.t"
#include "../main/topexprs.t"	/* pragmas */
#include "subst_t.t"
#include "prefix.t"
#include "subst.t"
#include "unify.t"
#include "conutil.t"
#include "check.t"

export Wdlh;

rec Wdlh St p dl defs u = Wdl St p defs (conc dl) u 

and prassert = prttype o as2ty

and consinst t u = instTR (getTvars t) u
-- Add name of defined entity to error message.
and addterr i (bad es) = 
    let s = idtostr i in
    let s2 = head 2 s in
    if s2 = "MM" | s2 = "DD" then
	-- Take care of methods etc.
	bad (('_'.reverse (fst (splitat '.' (reverse s)))) . es)
    else
	bad (s.es)
||  addterr _ S = S

-- test if a definition is overloadable
-- this is a hack.
and isovl (i, e) = ~OverloadRestr | isovle e | isovlid i
and isovle (Einfo overload _) = true
||  isovle (Einfo (restr _ _) _) = true
||  isovle (Einfo _ e) = isovle e
--||  isovle (Elet _ _ e) = isovle e		-- maybe we need this?
||  isovle (Elam _ _) = true
||  isovle _ = false
-- allow overloading for methods.
and isovlid (mkid _ ('M'.'M'._) _ _) = true
||  isovlid (mkid _ ('D'.'D'._) _ _) = true
||  isovlid _ = false

and normt S t =
    if cpart t ~= [] then fail ("normt "@prttype t) else
    let vs = getTvars t in
    xmkcontype (extractcon vs S) t

and addcx [] t = t
||  addcx cx (mktcontype cx' t) = mktcontype (cx@cx') t
||  addcx cx t = mktcontype cx t
and TRTcx T cx = cpart (T (mktcontype cx (mktcons dummyid [])))

and kvars (ok _ aas _) = mkset (getavars aas)
||  kvars _ = []

and
    chkdefs St p (z as (_,_,defs)) u ds =
	let! (u1, STs) =
	    mapstate (\u.\(a,b).
                      let (Rw,r,D,b',u1) = W St p z b u in
		      let Rr = resolvedefs Rw D defs (TRprefix Rw p) (getTvars r) in
		      let R = ecombTR b Rw Rr in
		      (u1, (addterr a R, r, TRdict Rr D, (a,  b'))))
		     u ds
	in
	let! (Ss, Ts, Ds, ds') = split4 STs in
let rrr =
	(u1, combTRs Ss, Ts, ds', Ds)
in
if TestN > 1 then trace (force ("chkdefs "@show_list (prid o fst) ds' @ show_list prttype Ts)) rrr else
rrr

and newdn it ng R ie r D u = 
    let curk = extractcon (difference (getTvars r) ng) R in
    let is = map2 buildid curk (from u) in
(if Test then trace ("newdn ng="@show_list show_int ng) else (\x.x))
(
    newd it R curk is ie r D false (u+length curk)
)

and unovl p ts R defs =
    let ng = getngs (TRprefix R p)
    and ts' = map (TRtype R) ts in
    let tvs = reduce union [] (map getTvars ts') in
    let curk = extractcon (difference tvs ng) R in
    let numk = filter (\ (mkassert ci _).member eqid ci numClass) curk in
    let S = resolvtyvars defs [] numk in
--trace ("unovl"@show_list prassert numk @ show_list prttype defs @ prTR R @ prpre p @ "ng="@show_list show_int ng @ "ts="@show_list prttype ts)
    S

and
    Wdlnorm St p (ps,it,defs) dl u =
      if length dl ~= 1 then
	fail ("Wdlnorm: strange dl")
      else
	let rec (u1, R0, xts, dl', Ds) = chkdefs St p (ps,it', defs) u dl
        and     R = combTR R0 Rx
        and     Rx = if NumOverload then emptyTR else unovl p xts R0 defs
        and     ts = map (TRtype R) xts
        and     it' = if noovl then it else reduce (\(i,e).\r.itadd1 r i e) it (conc iess)
        and     (xs, u2) = Umap3 (newdn it (getngs p') R) dl' ts Ds u1
	and     (dl'', Ds', ts', iess) = split4 xs
	and     p' = TRprefix R p
        and     noovl = ~all isovl dl in
	(if Test then trace ("Wdlnorm R0="@prTR R0) else (\x.x)) (
	if noovl then
	    Wlast dl dl'  Ds  ts  p' R u2
	else
	    Wlast dl dl'' Ds' ts' p' R u2
	)
	    

and 
    onedef (i, Einfo (restr _ t) _) _ & (UseRestr) = (true, (i, (t, getTvars t)))
||  onedef (i, _) t = (false, (i, (t, [])))
and
    -- There is an extra complication with recursive definitions and contexts:
    -- the recursive call has to be changed as well, and all of the functions need
    -- the transitive closure of the contexts.
    Wdlrec St p (ps,it,defs) dl u =
	let ul = u+length dl in
	let nt = for u (ul-1) mktvar in
        let (hasrestrs0, np) = split (map2 onedef dl nt) in
	let hasrestrs = if length dl = 1 then [false] else hasrestrs0 in
	let rec (u1, Rc, dTs, dl', Ds) = chkdefs St (addngens np p) (ps,it', defs) ul dl
	and      Ss = map (\(dT, T).Unify T dT) (combine (dTs, nt))
	and      R0 = combTRs (Ss @ [Rc])
        and      R = combTR R0 Rx
        and      Rx = if NumOverload then emptyTR else unovl p dTs R0 defs
        and      tts = map (TRtype R) dTs
        and      ries = map (\(i,_).(i, caply (Evar i) (map Evar is))) dl
	and      is = map2 buildid curk (from u1)
        and      curk = extractcon (difference (reduce union [] (map getTvars tts)) (getngs p')) R
        and      (xs, u2) = Umap4 (newd it R curk is) dl' tts Ds hasrestrs (u1 + length curk)
        and      (dl'', Ds', ts', iess) = split4 xs
        and      it' = if noovl then it else reduce (\(i,e).\r.itadd1 r i e) it (conc (ries.iess))
        and      p' = TRprefix R p
        and      noovl = /*length dl = 1 &*/ ~all isovl dl in
	if noovl then
	    Wlast dl dl'  Ds  tts p' R u2
	else
	    Wlast dl dl'' Ds' ts' p' R u2

and Wlast dl dl'' Ds' ts' p' R u2 =
        let pre = combine (map fst dl, ts') in			-- New prefix
        -- ng are those type vars that should not be considered generic because of overloading constraints
        let ng = reduce union [] (map2 (\d.\t.if isovl d then [] else intersect (difference (getTvars t) (getngs p')) (kvars R)) dl ts') in
        let np = addpreng pre p' ng in				-- Add new prefix to old, given certain non-generics
	let D = combdicts Ds' in				-- Combine dictionary mappings
        let R' = prunecontext (getngs np) R in
let rrr =
	    (R', np, D, dl'', u2)
in
if Test then trace (force("Wlast ng="@show_list show_int ng@"\nR="@prTR R@"\nR'="@prTR R'@"\nts'="@show_list prttype ts'@"\nnp="@prpre np@"\nD="@prD D)) rrr else
rrr

and argtypes (mktcons c [ta; tr]) & (eqid c hiARROW) = ta . argtypes tr
||  argtypes _ = []

and Dnil = []
and
    Wlam St p z (ta.ts) (Elam i d) u =
        --W St p z (Elam i d) u
	let! (R, r, D, d', u1) = Wlam St (addngens [(i, (ta, []))] p) z ts d u in
	(R, TRtype R (Tarr ta r), TRdict R D, Elam i d', u1)
||  Wlam St p z ts f u = W St p z f u
and
    W St p z f u =
let (rrr as (R, t, d, e, u)) = 
	case f in
	   Einfo (restr nvs t) e :
		let okvars = [] in
		let u1 = u in
		let! ((tn, u2), tvars, nvs') =
		    if null nvs then -- fast special case
			let! (tn, u2) = inst t (getTvars t) u1 in
			((tn, u2), count u1 (u2-1), [])
		    else
			let tvars' = difference (getTvars t) nvs @ nvs in
			let! (tn, u2) = inst t tvars' u1 in
			((tn, u2), count u1 (u2-1-length nvs), count (u2-length nvs) (u2-1))
		in
		let! (R, r, D, e', u3) = Wlam St p z (argtypes (tpart tn)) e u2 in
		let U = Unify r tn in
		let V = ecombTR f R U in
		-- check that restr. are not inst.
		let ngs = difference (getngs (TRprefix (if UseRestr&~(null nvs) then V else U) p)) okvars in
		let V1 = chkgen nvs' tvars V (normt V (TRtype V r)) tn ngs in     
-- HACK! Use U.  Type restriction is in the wrong place for recursive functions
-- Not even UseRestr works.  Using U makes certain illegal type restriction go through, which may cause
-- bad overloading translations resulting in a program crash!!!
											-- for recursive definitions.
#if 0
		let V1' = if null nvs then V1 else prunecontext (tvars @ [ v ;; mktvar v <- map (TRtype V1 o mktvar) tvars ]) V1 in
                let V2 = reorder V1' tn in
#else
		let V2 = if null nvs then reorder V1 tn else 
			     let V1' = prunecontext (tvars @ [ v ;; mktvar v <- map (TRtype V1 o mktvar) tvars ]) V1 in
			     let V1'' = reorder V1' tn in
			     let k = extractcon nvs' V1 in
			     case V1'' in
				ok kall k' ss : ok kall (k'@k) ss
			     || s : s
			     end
		in
#endif
		let tres = TRtype V2 (tpart tn) in
(if Test then traces ("restr\nV1="@prTR V1@"\nV2="@prTR V2@"\nnvs="@show_list show_int nvs@"\ntvars="@show_list show_int tvars@"\ntres="@prttype tres) else \x.x)
		(pruneTR u V2, tres, TRdict V2 D, e', u3)		-- use tn instead of r to get type in the asked for shape
	|| Einfo (trestr t) e :
		let! (R, r, D, e', u1) = W St p z e u in
		let U = Unify r t in
		let V = ecombTR f R U in
		let rt = synexpandall (TRtype St t) in
		let e'' = if ~UseSpecFunc then e' else
		    case getlspec z (leftid e') in
			None : e'
		    ||  Some its : 
			    let its' = mapfilter (\ (i,t).oapply ((i,t),) (matcht t rt)) its in
			    case sort (\ (_,l1).\ (_,l2).length l1 < length l2) its' in
				[] : e'
			    ||  (((i,t), Q)._) : 
				if null (cpart t)  then
				    Evar i
				else
				    fail ("Specialization with context not implemented yet: "@prid i)
			    end
		    end in
		(pruneTR u V, TRtype V r, TRdict V D, Einfo (trestr t) e'', u1)
	|| Einfo (srestr t) e :
		let! (R, r, D, e', u1) = W St p z e u in
		let U = Unify r t in
		let V = ecombTR f R U in
		case R in
		   ok _ (_._) _ : (pruneTR u V, TRtype V r, TRdict V D, Einfo (srestr t) e', u1)
		|| _ : (R, r, D, e', u1)
		end
	|| Einfo notchk e :				-- No typecheck here, assume a fresh variable
	        (emptyTR, mktvar u, Dnil, e, u+1)
	|| Einfo (dropctx t vs) (e as Evar i) :
		let! (typ, gl) = pfind i p in
		let rvs = case Unify t typ in bad _ : fail "dropctx cannot unify" || ok _ _ s : [ nv ;; (ov, mktvar nv) <- s; mem ov vs ] @ [ nv ;; (nv, mktvar ov) <- s; mem ov vs] end in
		let typ' = case typ in mktcontype k t : xmkcontype [mkassert c vs;; mkassert c vs <- k; null (intersect vs rvs)] t || t : t end in
		let p' = addngens [(i, (typ',gl))] p in
		let! (R, r, D, e', u1) = W St p' z e u in
		(if Test then trace ("dropctx "@prttype typ@","@prttype typ') else \x.x)
		(R, r, D, e', u1)
	|| Einfo f e :
		let! (R, r, D, e', u1) = W St p z e u in
		(R, r, D, Einfo f e', u1)
	|| Eap d e :
		let! (R, r, D1, d', u1) = W St p z d u in
		let! (S, s, D2, e', u2) = W St p z e u1 in
		let beta = mktvar u2 in
		let U = Unify r (Tarr s beta) in
		let V = ecombTR f U (combTR S R) in
--trace("apply: "@pr f@" :: "@prttype (normt V (TRtype V beta)))
--trace ("apply "@force (pr d@prTR R)@" and "@force (pr e @ prTR S) @ force (" U="@prTR U) @ force ("V="@prTR V))
		(pruneTR u V, TRtype V beta, TRdict V (combdict D1 D2), Eap d' e', u2+1)
	|| Evar i :
		let! (typ, gl) = pfind i p in
		let! (t, un) = inst typ gl u in
		let! (D, e', u') = De z t f un in
		let k = cpart t in
let rrr =
		(ok [] k [], tpart t, D, e', u')
in
--trace ("var "@pr f@" :: "@prttype t@" (was "@prttype typ@")")
if Test then trace ("Evar "@prid i@" "@prttype t@" was "@prttype typ@" dict ="@show_list (show_pair(prid, (show_pair(prid,prttype)))) D) rrr else
rrr
	|| Econstr (c as Cconstr str ctyp cinf no (uni,vs,cx) ts) es :
	    if uni & not (null vs) then
		let addcx' cx t = let vs = getTvars t in addcx [mkassert c vv ;; mkassert c vv <- cx; exists (\v.mem v vs) vv ] t in
		let rec elet [] e = e || elet (b.bs) e = Elet false [b] (elet bs e) in
		let is = for u (u + length es-1) (mknewid "uu") in
		let u = u+length es in
		let ets = map2 (\e.\ (t,_).Einfo (restr (difference (getTvars t) vs) (addcx' cx t)) e) es ts in
		let bs = combine (is,ets) in
		let newe =
		    elet bs (Econstr (Cconstr str ctyp cinf no (false,vs,[]) ts) (map2 (\ i . \ (t,_) . Einfo (dropctx t vs) (Evar i)) is ts)) in
(if Test then trace ("univ quant "@pr newe) else \x.x)
		W St p z newe u

	    else
		let! (T, u1) = instTR (vs@getTvars ctyp) u in
		let! (u2, SDes) =
		    mapstate (\u.\(t,e).let (R, r, D, e', u1) = W St p z e u in
					(u1, (ecombTR e (Unify r (T t)) R, D, e')))
			     u1
			     (combine (map fst ts, es))
		in
		let (Ss, Ds, es') = split3 SDes in
		let R = ecombTRs f Ss in
                let cx' = TRTcx T cx in
		let tn = addcx cx' (T ctyp) in
                let ctvs = concmap (getTvars o T o fst) ts in
		let ctn = (filter f (cpart tn) where f (mkassert _ vs) = all (\v.mem v ctvs) vs || f _ = true) in
		let (D, e', u3) = Dc z cx' (Econstr c) es' u2 in
--trace ("constr "@show_list show_int vs@" "@prttype tn)
--trace ("constr "@pr f@" :: "@prttype (TRtype R (mktcontype ctn (tpart tn))))
		(pruneTR u (addconTR ctn R), TRtype R (tpart tn), TRdict R (combdicts (D.Ds)), e', u3)
        || Ecfunction _ _ : (emptyTR, mktcons hiCPointer [], Dnil, f, u)
	|| Elam i d :
		let tv = mktvar u in
		let! (R, r, D, d', u1) = W St (addngens [(i, (tv, []))] p) z d (u+1) in
--trace ("lam "@pr f@" :: "@prttype (normt R (TRtype R (Tarr tv r))))
let rrr =
		(pruneTR u R, TRtype R (Tarr tv r), TRdict R D, Elam i d', u1)
in
if Test then trace ("lam "@prid i@" gets "@itos u) rrr else
rrr
	|| Efailmatch _ :
		(emptyTR, mktvar u, Dnil, f, u+1)
	|| Ecase e (ces as ((firstc, _ ,_)._)) de :
		-- T is used to instanciate the typevars for the constructor
    		let theType = ctype firstc in
		let! (T, u1) = consinst theType u in
		let! (R0, eT, De, e', u2) = W St p z e u1 in
		let R1 = echk e (Unify (T theType) eT) in
		let! (R2x, ResT, Dd, de', u3) = W St p z de u2 in
		let R2 = echk de R2x in
		let! (u4, Tes) =
	mapstate (\u.\ ((c as Cconstr _ _ _ _ (uni,vs,cx) ts), is, e).
            let (addp, ts', ss, cx', ids, u0) = 
				if uni & not (null vs) then
				    let (T1, u1) = instTR vs u in
				    let ngs = getTvars (T theType) in
				    let f pl p = addpreng pl p ngs in
--trace ("Ecase: "@prttype theType@" ---> "@ prttype (T theType))
				    (f, map (T1 o T o addcx cx o fst) ts, [], [], [], u1)
				else
				if null vs then (addngs, map (T o fst) ts, [], [], [], u) else 
				let fs = map mktvar (difference (reduce union [] (map (getTvars o fst) ts)) vs) in      -- free type vars in constr
				let n = length cx
				and m = length vs in
				let ids = for (u+m) (u+m+n-1) (mknewid "ED") in
				let ss = map2 (\v.\u.(v, skolemtype u fs (filter (\ (mkassert _ [v'],_).v=v') (combine (cx,ids))))) vs (from u) in   -- skolem subst -- ASSERT
				(addngs, map (T o TRtype (ok [] [] ss) o fst) ts, ss, TRTcx T cx, ids, u+m+n)
	    in
	    let! (V, r, D, e', u1) = W St (addp (combine(is, ts')) p) z e u0 in
	    -- third alternative happens when an srestr variable escapes
	    let U = if null vs | uni | chkskolem [] ss r V | chkskolem (getsrestrs e) ss r V then 
			Unify ResT r
		    else
			bad ["[93] Existential type escapes: "@nprttype r] in
--trace ("case "@show_list (show_pair(prid,prttype)) (combine(is,map (T o fst) ts)))
	    (u1, (ecombTR e U V, D, (c, ids@is, e'))))
	u3
	ces
		in
		let (Ts, Ds, ces') = split3 Tes in
		let S = ecombTRs f (Ts@[R2;R1;R0]) in
--trace (itos u@" "@prTR S)
		(pruneTR u S, TRtype S ResT, TRdict S (combdicts (De.Dd.Ds)), Ecase e' ces' de', u4)

	|| Elet r dl exp :
		let! (R, pref, D, dl', u1) = if r then Wdlrec  St p z dl u
				                  else Wdlnorm St p z dl u in
		let! (S, s, De, exp', u2) = W St pref z exp u1 in
		let V = ecombTR f S R in
		(pruneTR u V, s, TRdict V (combdict D De), Elet r dl' exp', u2)
	end
in
if Test then trace ("start "@pr f@force ("W f="@pr f@"t="@prttype t@"\np="@prpre p@"\nR="@prTR R@"\nD="@prD d)) rrr else
rrr

and prD d = show_list (show_pair(prid,show_pair(prid,prttype))) d

and Wdl St pre (ps,defs) dl u =
(if Test then trace ("Wdl scc="@show_list (show_pair(show_bool,(show_list (prid o fst)))) (sccds dl)) else (\x.x))
(
    let rec (rT, rp, ru, rD, rdss) = f emptyTR pre u Dnil [] (sccds dl)
    and     f T p u Dr odss [] = (T, p, u, Dr, reverse odss)
     ||     f T p u Dr odss ((r, ds).dss) =
                  let (R, p', D, ds', u') = if r then Wdlrec  St p (ps,topit, defs) ds u 
					         else Wdlnorm St p (ps,topit, defs) ds u in
	          let S = ecombTR (snd (hd ds)) T R in
--let r =
		  f S p' u' (combdict Dr D) (ds'.odss) dss
--in case S in bad _ : trace ("T="@prTR T@"\nR="@prTR R) r || _ : r end
    and     topit = reduce (\(i,e).\r.itadd1 r i e) (itnil 9) ies
    and     Rr = resolvedefs rT (TRdict rT rD) defs pre []	-- resolve everything on the top level to avoid problems
    and     T = combTR rT Rr
    and     rD' = TRdict T rD
    and     (ies, Dns) = ynsplit (map (handleD [] []) rD') [] []
    in
(if Test then trace ("Wdl rT="@prTR rT@"\nRr="@prTR Rr@"\nrD="@prD rD) else \x.x)
--trace ("Wdl rT="@prTR rT@"\nRr="@prTR Rr@"\nrD="@prD rD)
(
    if ~ (null Dns) & (case T in ok _ _ _ : true || _ : false end) then
	fail ("Unresolved in Wdl "@prD Dns)
    else
	(T, TRprefix Rr rp, ru, rdss)
)
)

and De (ps,it, def) t (e as Evar i) u =
--trace ("insert at "@pr e@" "@show_list prttype (cpart t))
(
    case cpart t in
	[] : ([], itlookupdef it i e, u)
    ||  k  : let n = length k in
	     let is = for u (u+n-1) (mknewid "D") in
	     (combine (is, map (\(mkassert d [v]).(d, mktvar v)) k), caply e (map (\i.itlookupdef it i (Evar i)) is), u+n) -- ASSERT
    end
)
and Dc _          [] f es u = ([], f es, u)
||  Dc (ps,it, _) k  f es u =
        let n = length k in
	let is = for u (u+n-1) (mknewid "D") in
	(combine (is, map (\(mkassert d [v]).(d, mktvar v)) k), f (map (\i.itlookupdef it i (Evar i)) is @ es), u+n)  -- ASSERT

and leftid (Evar i) = i
||  leftid (Eap f _) = leftid f
||  leftid (Einfo _ e) = leftid e
||  leftid e = fail ("No match in leftid: "@pr e)
and getlspec (ps,_,_) i =
	let its = assocdefeq eqid i ps [] @ 
		  concmap (concmap sspec o snd) (filter (\ (i', _).eqid i i') pragmas) in
--trace ("getlspec "@prid i@" = "@show_list (show_pair(prid,prttype)) its@"\n"@"ps="@show_list (show_pair(prid,show_list (show_pair(prid,prttype)))) ps)
	case its in
	   [] : oapply (map (\ (i,t,_).(i,t))) (getspec i)
	|| its : Some its
	end
and sspec (Pspecialize i [(t,_)]) = [(i, t)]
||  sspec _ = []

and getsrestrs :: Expr -> List Int
and getsrestrs e = getsrs e
and getsrs (Ecase e pl d) = getsrs e @ conc (map (getsrs o thd) pl) @ getsrs d
||  getsrs (Elet _ ds e) = conc (map (getsrs o snd) ds) @ getsrs e
||  getsrs (Econstr _ es) = concmap getsrs es
||  getsrs (Efailmatch _) = []
||  getsrs (Einfo (srestr (mktvar u)) e) = [u]
||  getsrs (Einfo (trestr (mktvar u)) e) = [u]
||  getsrs (Einfo _ e) = getsrs e
||  getsrs (Evar _) = []
||  getsrs (Eap f a) = getsrs f @ getsrs a
||  getsrs (Elam _ e) = getsrs e
||  getsrs e = fail ("getsrs "@pr e)

and traces s x = if s=s then trace s x else fail "???"

end
