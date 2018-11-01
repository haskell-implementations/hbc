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
#include "../expr/id.t"
#include "../expr/constrfun.t"
#include "../ExprE/Expr_t.t"
#include "../ExprE/Eprint.t"
#include "../ExprE/Egetid.t"
#include "../ExprE/unrec.t"
#include "../transform/misc.t"
#include "../misc/misc.t"
#include "../misc/flags.t"
#include "subst_t.t"
#include "prefix.t"
#include "subst.t"
#include "unify.t"

export Wdll, echk, ecombTRs, ecombTR;

rec Wdll p b u = let (S, np, b', u) = Wdl p (conc b) u in (S, np, u, b')
and echk f (bad [s]) = bad [' '.pr f; s]
||  echk _ t = t
and ecombTR f s1 s2 = echk f (combTR s1 s2)
and ecombTRs f Ss = echk f (combTRs Ss)
and cst ((c, _ ,_)._) = ctype c
and consinst c u = instTR (getTvars (cst c)) u
and addterr i (bad es) = bad (idtostr i.es)
||  addterr _ S = S
and
    tagdef pre (i, e as Elam _ _) = (addtype i (pfind i pre), e)
||  tagdef _ ie = ie
and
    addtype (mkid n s (idi_var vi _ osel) on) (t, tv) = mkid n s (idi_var vi (Ohastype t tv None/*!!*/) osel) on
||  addtype i _ = i
and
    chkdefs p u ds =
	let! (u1, STs) =
		mapstate (\u.\(a,b).let! (R,r,e',u1)=W p b u 
				    in (u1,(addterr a R,r,(a,e')))) u ds
	in
	let! (Ss, Ts, ds') = split3 STs in
	(u1, combTRs Ss, Ts, ds')
and
    Wdlnorm p dl u =
	let! (u1, R, ts, dl') = chkdefs p u dl in
	let pre = combine (map fst dl, map (TRtype R) ts) in
	let p1 = addpre pre (TRprefix R p) in
	(R, p1, map (tagdef p1) dl', u1)
and 
    onedef (i, Einfo (restr _ t) _) _ &(UseRestr) = (i, (t, getTvars t))
||  onedef (i, _) t = (i, (t, []))
and
    Wdlrec p dl u =
	let ul = u+length dl in
	let nt = for u (ul-1) mktvar in
        let np = map2 onedef dl nt in
	let! (u1, R, dTs, dl') = chkdefs (addngens np p) ul dl in
	let Ss = map (\(dT, T).Unify T dT) (combine (dTs, nt)) in
	let R = combTR (combTRs Ss) R in
	let pre = combine (map fst dl, map (TRtype R) dTs) in
	let p1 = addpre pre (TRprefix R p) in
	(R, p1, map (tagdef p1) dl', u1)
and
    W p f u =
	case f in
	   Einfo (restr _ t) e :
		let! (R, r, e', u1) = W p e u in
		let! (tn, u2) = inst t (getTvars t) u1 in
		let U = Unify r tn in
		let V = ecombTR f R U in
		-- check that restr. are not inst.
		let V1 = chkgen [] (count u1 (u2-1)) V r tn [] in
		(pruneTR u V1, TRtype V1 tn, e', u2)
	|| Einfo (trestr t) e :
		let! (R, r, e', u1) = W p e u in
		let U = Unify r t in
		let V = ecombTR f R U in
		(pruneTR u V, TRtype V r, Einfo (trestr t) e', u1)
	|| Einfo notchk e :				-- No typecheck here, assume a fresh variable
	        (emptyTR, mktvar u, e, u+1)
	|| Einfo f e :
		let! (R, r, e', u1) = W p e u in
		(R, r, Einfo f e', u1)
	|| Eap d e :
		let! (R, r, d', u1) = W p d u in
		let! (S, s, e', u2) = W p e u1 in
		let beta = mktvar u2 in
		let U = Unify r (Tarr s beta) in
		let V = ecombTR f U (combTR S R) in
		(pruneTR u V, TRtype V beta, Eap d' e', u2+1)
	|| Evar i :
		let! (typ, gl) = pfind i p in
		let! (t, un) = inst typ gl u in
		(emptyTR, t, f, un)
	|| Econstr (ct as Cconstr _ ctyp _ _ (_,vs,_) ts) es :
		let! (T, u1) = instTR (vs@getTvars ctyp) u in
		let! (u2, eSs) =
		    mapstate (\u.\(t,e).let (R, r, e', u1) = W p e u in
					(u1, (e', ecombTR e (Unify r (T t)) R)))
			     u1
			     (combine (map fst ts, es))
		in
		let (es', Ss) = split eSs in
		let R = ecombTRs f Ss in
		(pruneTR u R, TRtype R (T ctyp), Econstr ct es', u2)
	|| Elam i d :
		let tv = mktvar u in
		let! (R, r, d', u1) = W (addngens [(i, (tv, []))] p) d (u+1) in
		(pruneTR u R, TRtype R (Tarr tv r), Elam i d', u1)
	|| Efailmatch _ :
		(emptyTR, mktvar u, f, u+1)
	|| Ecase e ces de :
		-- T is used to instanciate the typevars for the constructor
		let! (T, u1) = consinst ces u in
		let! (R0, eT, e', u2) = W p e u1 in
		let R1 = Unify (T (cst ces)) eT in
		let! (R2, ResT, de', u3) = W p de u2 in
		let! (u4, Tes) =
	mapstate (\u.\((c as Cconstr _ _ _ _ (_,vs,_) ts), is, e).
            let (ts', ss, u0) = if null vs then (map (T o fst) ts, [], u) else 
				let fs = map mktvar (difference (reduce union [] (map (getTvars o fst) ts)) vs) in      -- free type vars in constr
				let ss = map2 (\v.\u.(v, skolemtype u fs [])) vs (from u) in   -- skolem subst
				(map (T o TRtype (ok [] [] ss) o fst) ts, ss, u+length vs)
	    in
	    let! (V, r, e', u1) = W (addngs (combine(is, ts')) p) e u0 in
	    let U = if null vs | chkskolem [] ss r V then Unify ResT r else bad ["[93] Existential type escapes: "@nprttype r] in
	    (u1, (ecombTR e U V, (c, is, e'))))
	u3
	ces
		in
		let (Ts, ces') = split Tes in
		let S = ecombTRs f (Ts@[R2;R1;R0]) in
		(pruneTR u S, TRtype S ResT, Ecase e' ces' de', u4)

	|| Elet r dl e:
		let! (R, pref, dl', u1) = if r then Wdlrec p dl u
				         else Wdlnorm p dl u in
		let! (S, s, e', u2) = W pref e u1 in
		(pruneTR u (ecombTR f S R), s, Elet r dl' e', u2)
	end

and Wdl pre dl u = f emptyTR pre u [] (sccds dl)
    where rec f T p u xs [] = (T, p, reverse xs, u)
           || f T p u xs ((r, ds as (_,e)._).dss) =
	let! (R, p', ds', u') = if r then Wdlrec p ds u else Wdlnorm p ds u in
	f (ecombTR e T R) p' u' (ds'.xs) dss
end
