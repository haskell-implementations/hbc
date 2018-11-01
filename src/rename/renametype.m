module -- renametype
--
-- do type renaming
--
#include "../misc/triple.t"
#include "../expr/id_t.t"
#include "../expr/constr_t.t"
#include "../expr/ttype_t.t"
#include "../expr/einfo_t.t"
#include "../expr/types_t.t"
#include "../expr/ttype.t"
#include "../expr/id.t"
#include "../misc/misc.t"
#include "../misc/util.t"
#include "../misc/flags.t"
#include "../transform/lettrans.t" /* just andify */
#include "../transform/hexpr.t"
#include "renamedef.t"
#include "renameutil.t"
#include "renenv.t"
#include "classutil.t"
/*#include "deriv.t"*/

export rentype, renbtype, renbt, renbtenv, rensyntype, rencontext, syncirc,
       renbctype, renbct;
rec rentype env tt = 
        (if X4 then trace ("rentype "@prttype tt) else (\x.x))
	(rentype1 env tt)
and rentype1 env tt =
	case tt in
            mktcons (mkid _ _ _ _) _ : tt
	||  mktcons i ltt :
	        case rfindid Ktype i env in
		    mkid 0 _ _ _ :  
			if Curry then
			    mkterror ("[22] Undefined type "@oprid i)
			else
			    mktcons (mkid 0 (idtostr i) idi_udef noorigname) (map (rentype1 env) ltt)
		|| (id as (mkid _ _ (idi_type _ _ n _ _ _) _)) :
			if length ltt = n | HigherKind then
		            mktcons id (map (rentype1 env) ltt)
			else
			    mkterror ("[23] Bad type arity "@oprid i)
		|| (id as (mkid _ _ (idi_conctype _ _) _))  :
		           mktcons id (map (rentype1 env) ltt)
		|| (id as (mkid _ _ (idi_syn _ src n dst) _)) :
			if length ltt = n then
			    mktcons id (map (rentype1 env) ltt)
			else
			    mkterror ("[24] Bad type synonym arity "@oprid i)
		|| _ : mkterror ("[50] Not a type "@oprid i)
		end
	|| mktap v ltt : mktap v (map (rentype1 env) ltt)
	|| mktvar _ : tt
	|| mktcontype ts t : 
	       if null (difference (concmap (\ (mkassert _ v).v) ts) (getTvars t)) then
		   mktcontype (map (rencontext env) ts) (rentype1 env t)
	       else
		   mkterror ("[84] Unused type var in context: "@prttype tt)
	|| mkterror _ : tt
	end

and rencontext env tt =
	case tt in
            mkassert (mkid _ _ _ _) _ : tt
	||  mkassert i vs :
	        case rfindid Ktype i env in
		    mkid 0 _ _ _ :  
			    mkaerror ("[25] Undefined class "@oprid i)
		|| (id as (mkid _ _ (idi_class _) _)) :
		            mkassert id vs
		|| _ : mkaerror ("[26] Not a class "@oprid i)
		end
	end
and rensel env None = None
||  rensel env (Some (mkids s)) = Some (rfind Kvalue s env)
and renbt localq pref ff env u tt atl =
--trace ("renbt type "@prttype tt) 
(
        let (mktcons tyid vs) = tpart tt in
        let bvs = getTvars tt in
        let ucvs = difference (mkset (getavars (cpart tt))) bvs in
	let uvs = difference (reduce (union o getTvars) [] (concmap (\(mkcons _ _ tbs _).map fst3 tbs) atl)) bvs in
        if ~ localq & ~ null uvs | ~ null ucvs then
	    (u, mkterror ("[27] Unbound type var(s) "@show_list (prttype o mktvar) uvs@" in "@prttype tt), [])
	else if anysame vs then
	    (u, mkterror ("[28] Duplicate type var(s) in "@prttype tt), [])
        else if ~ localq & exists (\(mkcons _ (_,vs,_) _ _).not (null vs)) atl then
	    (u, mkterror ("[94] Local quantification not allowed: "@prttype tt), [])
        else
        let exvars ts = if ~ localq then [] else difference (mkset (concmap (getTvars o fst3) ts)) bvs in
	let ttn = rentype env tt in
	let rec atln = map2 (\n.\(mkcons i (uni,_,c) ts flg).
	    let tsn = map (\ (t,b,osel) . (rentype env t, b, rensel env osel)) ts in
	    let s = setpref pref (idtostr i) in
            let e = (uni,exvars ts, map (rencontext env) c) in
            let rec ii =  mkid (u+n) s (idi_constr ttn e tsn n atln flg None) (ff xi)
		and xi =  mkid (u+n) s bot (Orignames bot bot (MI (getminame (fst (id_orignames (tname ttn)))), snd (id_orignames i)))
		and bot = fail "xi-constr"
            in
--trace ("renbt "@dprid i@" --> "@dprid ii)
	    (mkcons ii e tsn flg))
		        (from 0) atl
	in (u+length atl, ttn, atln)
)
and renbtenv atln = (rlist Kvalue (concmap rene atln)
		where rene (mkcons (i as mkid no s ii on) _ _ _) = [i]
		   || rene _ = [])

and isflat ats = all (\(mkcons _ _ xs _).length xs = 0) ats

-- Deriving info is handled later when all is known
and renbtype ff env0 u (mkbtype ott atl ds iso) =
    case tpart ott in
	(mktcons (mkids s) tts) : 
	let rec tenv = rone Ktype iii
	and        k = rgetkind iii env0
        and      iii = mkid u s (idi_type k ttn (length tts) ti (gettypeinsts env0 iii) dsx) (ff iii)
        and       ti = mktinfo ttn (length atln) false (isflat atln) atln (hasext atln) iso' None
        and      iso' = iso | ~NewType & case atl in [mkcons _ (_,[],[]) [(_,s,_)] _] : s || _ : false end
	and      env = rjoin tenv env0
	and      (u1, ttn, atln) = renbt LocalQuant "" ff env (u+1) ott atl
        and      dsx = oapply (map (\i.rfindid Ktype i env0)) ds 
        and       nc = filter (not o id_isderclass) (gsome dsx)
        and       b' = mkbtype ttn atln dsx iso
	and     errc = difference (map oprid (gsome ds)) (map oprid (filter id_isderclass (gsome dsx)))
	in addselect ff env0 atl iii (u1, (if null nc then b' else mkberror ("[29] Cannot derive: "@mix errc ", ")), rjoin tenv (renbtenv atln))
	
    end
and mkapl s es = reduce (\a.\f.mkap f a) s (reverse es)
and addselect ff env0 ats ti (x as (u, b, env)) & (Record & MakeSelectors) =
	case mkset [ i ;; (mkcons x1 x2 tbis x3) <- ats; (t,b,Some i) <- tbis ] in
	   [] : x
	|| selids :
	        let us = mkident (mkids "_") and x = mkident (mkids "ss") in
	        let mksel i = mkbpat [(mkap (mkident i) 
				            (mkapl (mkident c) 
					           [if s = Some i then x else us;;
						    (t,b,s) <- tbs]), x) ;;
				      (mkcons c k tbs flag) <- ats; exists (\ (_,_,s) . s = Some i) tbs ] in
		let sels = andify (map mksel selids) in
		let (u', sels', senv0) = rendef dummyid ff env u sels in
		let setsel (mkid i s (idi_var v ot _) on) = mkid i s (idi_var v ot (Some ti)) on in
		let senv = rlist Kvalue (map setsel (rids Kvalue senv0)) in
		(u', mkband b sels', rjoin senv env)
        end
||  addselect _ _ _ _ x  = x

and rensyntype ff env0 u (mkbsyn src dst) =
    let (tsrc as mktcons (mkids s) vs) = tpart src in
    let rec tenv = rone Ktype iii
    and        k = rgetkind iii env0
    and      iii = mkid u s (idi_syn k src' (length (getTvars tsrc)) dst') (ff iii)
    and     src' = rentype (rjoin tenv env0) src
    and     dst' = rentype env0 dst in
    let msg = prttype src in
    let b = 
	if difference (getTvars dst) (getTvars tsrc) ~= [] | anysame vs then
	    mkberror ("[30] Synonym has bad type var: "@msg)
	else
	    case dst in
		mktcons _ _ : mkbsyn src' dst'
            ||  mktvar _  : mkbsyn src' dst'       -- ?? why not ?
	    ||  _ : mkberror ("[31] Bad synonym definition: "@msg)
	    end
    in (u+1, b, tenv)

-- Check that the environment contains no circular type synonyms.
and syncirc env =
    let is = filter id_issyn (rids Ktype env) in
    let g = map (\(i as mkid _ _ (idi_syn _ _ _ dst) _).(i, filter (\i.member eqid i is) (gettids dst))) is in
--trace (show_list (show_pair(prid,show_list prid)) g)
(
    hascycleeq eqid g
)
and gettids (mktcontype _ t) = gettids t
||  gettids (mktcons c l) = c.concmap gettids l
||  gettids (mktap _ l) = concmap gettids l
||  gettids _ = []

-- everything below this is for conctypes

-- fltterm   : List Cgs -> List Ttype
--             gives a list of the types in the list of Cgs:s
-- ctt       : Renv -> Cgs
--             type renaming on nonterminals
-- stripprod : Prod -> List Cgs
--             remove mknormal or mkforget constructor
-- renbct    : (Id -> Origname) -> Renv -> Int -> Ttype -> List Prod ->
--             (Int # Ttype # List Atype # (Ttype # (List (IdorConstr # Prod))))
--             makes among other things constructor names for conctypes 
-- renbctype : (Id -> Origname) -> Renv -> Int -> Binding ->
--             (Int # Binding # Renv)
and renbct ff env u (tt as mktcons _ vs) prods =
   let rec 
       spectype (mktcons (mkid _ "_Number" _ _) []) = Tint
   ||  spectype (mktcons (mkid _ "_Ident" _ _) [])  = Tstring
   ||  spectype t = t in
   let rec 
       fltterm []                 = [] 
   ||  fltterm (mkcnt t .l)       = spectype t. fltterm l
   ||  fltterm (mkct te.l)        = fltterm l 
   ||  fltterm (mkctint te.l)     = fltterm l 
   ||  fltterm (mkctid te.l)      = fltterm l 
   ||  fltterm (mkctsym te.l)     = fltterm l 
   ||  fltterm (mklist1 t _ _ .l) = Tlist (spectype t). fltterm l
   ||  fltterm (mklist0 t _ .l)   = Tlist (spectype t). fltterm l  in
   let rec
       ctt env (mkct t)            = (mkct t)
   ||  ctt env (mkctint t)         = (mkctint t)
   ||  ctt env (mkctid t)          = (mkctid t)
   ||  ctt env (mkctsym t)         = (mkctsym t)
   ||  ctt env (mkcnt nt)          = mkcnt (rentype env nt)
   ||  ctt env (mklist1 nt cgsl n) = mklist1 (rentype env nt) cgsl n
   ||  ctt env (mklist0 nt cgsl)   = mklist0 (rentype env nt) cgsl  in 
					-- cgsl are just terminals
   let
       stripprod (mknormal cgsl _) = cgsl 
   ||  stripprod (mkforget cgsl _) = cgsl in
   if difference (reduce (union o getTvars)
			 []
			 (concmap (fltterm o stripprod) prods)
		 )
		 (getTvars tt) ~= [] | anysame vs
   then
	    (u, mkterror "[32] Bad type var", [],(mktvar 0,[]))
   else
	let (ttn as mktcons (mkid _ ctname _ _) _) = rentype env tt in
        let rec mkconstrname n (mknormal lcgs p) = 
              (let nname = ctname @ "-" @ show_int n in
              let nlcgs = map (ctt env) lcgs in 
              let narg = map (\x.(x,false,None)) (fltterm nlcgs) in
              let rec madeconst = mkid (u+2*n)
				       nname
				       (idi_constr ttn (false,[],[]) narg n atln false None)
				       (ff madeconst)
              in (mkcons madeconst (false, [], []) narg false, (cid madeconst, mknormal nlcgs p)))
            ||  mkconstrname n (mkforget lcgs p) = 
              (let nname = ctname @ "-" @ show_int n in
              let nlcgs = map (ctt env) lcgs in 
              let narg = map (\x.(x,false,None)) (fltterm nlcgs) in
              let rec madeconst = mkid (u+2*n)
				       nname
				       (idi_constr ttn (false,[],[]) narg n atln false None)
				       (ff madeconst)
              in (mkcons madeconst (false, [], []) narg false, (cid madeconst, mkforget nlcgs p)))
        and (atln,ncgs) = split (map2 mkconstrname (from 0) prods)
        in (u+2*length atln, ttn, atln, (ttn, ncgs))

and renbctype ff env0 u (mkbctype (tt as (mktcons (mkids s) tts)) prods) =
	let rec tenv = rone Ktype iii 
	and     iii  = mkid u s (idi_type mkkground ttn (length tts) ti (gettypeinsts rnil iii) None) (ff iii)
	and     ti   = mktinfo ttn (length atln) false (isflat atln) atln (hasext atln) false None
	and     env  = rjoin tenv env0 
	and     (u1,ttn,atln,ctenvobj) = renbct ff env (u+1) tt prods
	and	tenv' = rone Kvalue iiii
	and 	env'  = rjoin tenv' env
	and	iiii  = mkid u1 s (idi_conctype ttn prods) (ff iiii) 
	in (u1+1,mkbtype ttn atln None false,
	    rjoin tenv' (rjoin (rjoin tenv (renbtenv atln)) (rcone ctenvobj))
	   )

and id_isderclass i = 
	member eqid i ([hiEq; hiOrd; hiIx; hiEnum] @ 
		       if H1_3 then
			   [hiBounded; hiShow; hiRead; hiEval]
		       else
			   [hiBinary; hiText])
end

