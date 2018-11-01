module -- subst
#include "../expr/id_t.t"
#include "../expr/constr_t.t"
#include "../expr/id.t"
#include "../expr/ttype_t.t"
#include "../expr/ttype.t"
#include "../ExprE/Expr_t.t"
#include "../misc/flags.t"
#include "../misc/util.t"
#include "subst_t.t"
#include "unify.t"
#include "conutil.t"
/*import Unify:	(Ttype->(Ttype->Subst));*/

export emptyTR, addTR, combTR, combTRs, pruneTR, inst, TRtype, instTR, prTR, chkgen, addconTR, TRdict, extractcon, prunecontext, normtypes, nprttype, skolemtype, chkskolem, isskolemtype, lTRtype, usekall;

rec
-- The list in an substitution is always ordered, highest variable first.
-- The substitution is always idempotent, ie. nothing happens when applying
-- it to itself.
    emptyTR = ok [] [] []
and pruneTR n (ok kall k t) = ok kall k (filter (\(a,b).a<n /*| TestN > 1*/) t)
 || pruneTR _ s = s
and prunecontext vs (S as ok _ [] _) = S
||  prunecontext vs (ok kall aas t) =
    if AutoSpecialize then
	case partition (\(mkassert _ xs).all (\v.mem v vs) xs) aas in
	   (k1, k2) : ok (k2@kall) k1 t
	end
    else
	ok [] (filter (\(mkassert _ xs).all (\v.mem v vs) xs) aas) t
||  prunecontext _ S = S
and usekall (ok kall k s) = ok [] (kall@k) s
||  usekall s = s
and isgtvar (mktvar v) ngs = ~ mem v ngs
||  isgtvar _ _ = false
--and isgtvar t ngs = ~ exists (\v. mem v ngs) (getTvars t)
and implies a b = ~a | b
and chkgen nvs ts (s as ok _ k ss) t1_0 t2_0 ngs = 
let n = length nvs in -- XXXXX WRONG!
        let t1 = synexpandall t1_0 and t2 = synexpandall t2_0 in
        let newconvars = difference (getavars k) (getavars (cpart t2)) in
	let new = synexpandall (TRtype s (tpart t2)) in
	let tsl = length ts in
	let ntsl = length (getTvars new) in
let xxvars = intersect (mkset (concmap (getTvars o synexpandall o TRtype s o mktvar) nvs))
    		       (mkset (concmap (getTvars o synexpandall o TRtype s o mktvar) ts)) in
	let b1 = all (\(a,b).implies (mem a ts) (isgtvar (synexpandall b) ngs)) ss
	and b2 = null (intersect ts newconvars)
	and b3 = tsl <= ntsl & ntsl <= tsl + n 
	and b4 = null (intersect (getTvars t2) ngs) in
(if X3 then (trace ("chkgen\n"@
		    "nvs="@show_list show_int nvs @"\n"@
		    "ts="@show_list show_int ts @"\n"@
		    "s="@prTR s @"\n"@
		    "t1="@prttype t1@"\n"@
		    "t2="@prttype t2@"\n"@
		    "ngs="@show_list show_int ngs@"\n"@
		    "xx1="@show_list show_int (concmap (getTvars o synexpandall o TRtype s o mktvar) nvs) @"\n"@
		    "xx2="@show_list show_int (concmap (getTvars o synexpandall o TRtype s o mktvar) ts) @"\n"@
		    "new="@prttype new@"\n"@
		    "newconvars="@show_list show_int newconvars@"\n"@
		    "xxvars="@show_list show_int xxvars@"\n"@
		    "[b1,b2,b3,b4]="@show_list show_bool [b1;b2;b3;b4]
		   )) else (\x.x))
(
	if b1 & 		-- nothing has become non-generic
	   b2 & 		-- ?
           b3 &
	   b4 &
	   null xxvars
	then		-- no type vars identified
		s
	else
		let msg = if ~b1 | ~b4 then "non-generic tyvar" else if ~b2 then "new context tyvar" else "identified tyvars" in
		let [t1; t2] = normtypes [t1; t2] in bad ["[59] Bad restriction\n    "@prttype t2@"\nof type\n    "@prttype t1@"\n"@msg@"\n"]
)
||  chkgen _ _ s _ _ _ = s
#if 1
and prTR (ok _ k x) = show_list (prttype o as2ty) k @ show_list (\(a,b).(itos a)@"==>"@prttype b) x
 || prTR (bad s) = "Bad subst "@mix s ", "@"\n"
#endif
-- TRtype' is used very much, so use explicit recursion instead of Typerec
and TRcon ss k = flatcollaps (map (TRcon1 ss) k)
and TRcon1 ss (mkassert ci [v]) = (ci, assocdef v ss (mktvar v)) -- ASSERT
and TRcon11 v t (mkassert ci [v1]) = (ci, if v = v1 then t else mktvar v1)
and TRtype' ss (mktcontype k t) = zmktcontype (TRcon ss k) (TRtype' ss t)
||  TRtype' ss (mktcons i ts) = mktcons i (map (TRtype' ss) ts)
||  TRtype' ss (t as mktvar v) = assocdef v ss t
||  TRtype' ss (mktap v ts) =
        let ts' = map (TRtype' ss) ts
	in  case assocdef v ss (mktvar v) in
	       mktvar n' : mktap n' ts'
	    || mktcons i ts : mktcons i (ts@ts')
	    || mktap n ts : mktap n (ts@ts')
	    || t : fail ("TRtype' ap "@prttype t)
	    end
||  TRtype' ss t = fail ("Bad TRtype' "@prttype t)
and lTRtype ss t = TRtype' ss t

and zmktcontype (No msg) _ = mkterror msg
||  zmktcontype (Yes k) t = xmkcontype k t

-- special case: substitute for one variable
and /*TRtype1 v tn (mktcontype ts t) = mktcontype (map (TRtype1 v tn) ts) (TRtype1 v tn t)
||*/  TRtype1 v tn (mktcons i ts) = mktcons i (map (TRtype1 v tn) ts)
||  TRtype1 v tn (t as mktvar v1) = if v = v1 then tn else t
||  TRtype1 v tn (mktap v1 ts) =
        let ts' = map (TRtype1 v tn) ts
	in  if v = v1 then
	    case tn in
	       mktvar n' : mktap n' ts'
	    || mktcons i ts : mktcons i (ts@ts')
	    || mktap n ts : mktap n (ts@ts')
	    || t : fail ("TRtype1 ap "@prttype t)
	    end
	    else
	    mktap v1 ts'
||  TRtype1 v tn t = fail ("Bad TRtype1 "@prttype t)
and TRtype (ok _ _ ss) t = TRtype' ss t
 || TRtype _ t = t
and TRsubst s t = mapsnd (TRtype' s) t
-- special case: substitute for one variable
and TRsubst1 v t ss = mapsnd (TRtype1 v t) ss
and insTR s ss = /*trace ("insTR "@prTR (ok [] [] [s])@" in "@prTR (ok [] [] ss))*/ (insTR' s ss) 
and insTR' s [] = [s]
 || insTR' (s1 as (v1,_)) (ss as ((s2 as (v2,_)).l2)) = 
 	if (v1 > v2) then
		s1.ss
	else -- v1 < v2, because v1 = v2 is filtered out by addTR
		s2.insTR' s1 l2
-- combTR should be improved, it has horrible complexity!
and combTR (ok kall k a) b = addkall kall (addconTR k (reduce addTR' b a))
 || combTR (S as bad a) _ = S
 || combTR _ (S as bad a) = S

and addkall (kall as (_._)) (ok kall' k s) = ok (kall@kall') k s
||  addkall _ S = S

and combTRs [S] = S
||  combTRs Ss = reduce combTR emptyTR Ss

and addTR' (v,t') (ok kall k' s') =
    let t = TRtype' s' t' in
    let s = TRsubst1 v t s' in
    case flatcollaps (map (TRcon11 v t) k') in
        Yes k :
	    case assocdef v s (mktvar 0) in
		-- not found, this is easy
		mktvar 0 : if mem v (getTvars t) then
		               badu "[61] occurence" (mktvar v) t 
			   else 
			       ok kall k (insTR (v,t) s)
		-- found it, must unify.
	    || t' : case Unify t t' in
		        ok _ [] [] : ok kall k s	-- speedup only
		    || (S as ok _ _ _) : combTR S (ok kall k s)
		    || x : x		-- bad
		    end
	    end
    || No msg : bad [msg]
    end
 || addTR' _ s = s			-- bad
and addTR a s = --trace ("addTR "@prTR (ok [] [] [a])@" to "@prTR s)
(
combTR (ok [] [] [a]) s
)
-- This is used a lot, efficiency could be better.
and instTR gl u = let l = length gl in
                  let al = map2 (\g.\v.(g, mktvar v)) gl (from u) in
		  let ali = map2 (\g.\v.(g,v)) gl (from u) in
		  ((\t.case t in
			  mktcontype aas st :
			     let f (mkassert ci vs) = mkassert ci (map (\v.assocdef v ali v) vs)
			     ||  f x = fail ("instTR: "@prttype (as2ty x))
			     in mktcontype (map f aas)
	   (tsubst al st)
		       ||  _ : tsubst al t
		       end), u+l)
and inst typ [] u = (typ, u)
 || inst typ gl u = let! (T, un) = instTR gl u in (T typ, un)

-- Context stuff
and addconTR [] S = S
||  addconTR _  (S as bad _) = S
||  addconTR k (ok kall kk ss) = 
        case TRcon ss k in
	    Yes k : ok kall (combcon k kk) ss
        ||  No msg : bad [msg]
	end
and TRdict S D = map (\(i,(d, t)).(i, (d, TRtype S t))) D
and extractcon vs (bad _) = []
||  extractcon vs (ok _ ks _) = filter (\(mkassert _ xs).all (\v.mem v vs) xs) ks

--and getallcontext (ok kall _ _)

and normtypes ts =
	let tvs = getTvars (mktcons dummyid (map tpart ts)) in
        let al = combine (tvs, map mktvar (from 0)) in
	map (TRtype' al) ts

and nprttype t = prttype (normtype t)

and skolemtype u vs k = 
        let rec cls = map (\(mkassert ci _, i).(i, mkidecl [] ci [t] "__")) k -- ASSERT
        and     t = mktcons (mkid u ("SS"@itos u) (idi_type mkkground t (length vs) ti cls None) noorigname) vs
	and     ti = mktinfo err 0 false false [] false false None
	and     err = fail "skolem typeinfo"
        in  t
and hasskolem ss t = 
	Typerec (\_.false) (\c.\bs.member eqid c ss | Or bs) (\_.\bs. Or bs) t
and chkskolem nochkvars ss r (reals as ok _ _ s) = 
	let ss' = (map (\(_,mktcons i _).i) ss)
	and s' = filter (\ (i, _) . ~ mem i nochkvars) s
	in  ~ExistEscapeCheck | ~ (hasskolem ss' r | exists (hasskolem ss' o snd) s')
||  chkskolem _ _ _ _ = true
and isskolemtype (mkid _ ('S'.'S'._) _ _) = true
||  isskolemtype _ = false

end
