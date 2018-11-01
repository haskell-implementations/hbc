module
#include "../misc/triple.t"
#include "../misc/flags.t"
#include "../misc/misc.t"
#include "../expr/error.t"
#include "../expr/constr_t.t"
#include "../expr/constrfun.t"
#include "../expr/types_t.t"
#include "../expr/id_t.t"
#include "../expr/id.t"
#include "../expr/ttype_t.t"
#include "../expr/ttype.t"
#include "../expr/tinfo.t"
#include "../expr/impexp_t.t"
#include "../expr/impexp.t"
#include "../expr/pprint.t"
#include "../expr/einfo_t.t"
#include "../rename/renamedef.t"
#include "../rename/renameutil.t"
#include "../rename/renenv.t"
#include "../rename/import.t"
#include "../rename/deriv.t"
#include "../rename/buildinsts.t"
#include "../rename/rename.t"
#include "../transform/match.t"
#include "../transform/constr.t"
#include "../transform/lettrans.t"
#include "../transform/misc.t"
#include "../transform/exprs.t"
#include "../transform/remsign.t"
#include "../transform/remclass.t"
#include "../transform/remlazy.t"
#include "../strict/strictcon.t"
#include "../ExprE/Expr_t.t"
#include "../ExprE/Ecnv.t"
#include "../ExprE/Eprint.t"
#include "../ExprE/apconv.t"
#include "../ExprE/unrec.t"
#include "../ExprE/Eutil.t"
#include "../ExprE/classtrans.t"
#include "../ExprE/predef.t"
#include "../type/tchk.t"
#include "../curry/curry0.t"
#include "../zf/lmlzf.t"
#include "../zf/remzf.t"
#include "cexpr.t"
#include "iabs.t"
#include "id.h"
#include "imisc.t"
#include "econv.t"
#include "../funnos.h"
export icomp, i_preenv, i_unum1, i_ipreenv, ivectorwrap;
rec (i_unum1, insts, i_preenv, _, _, _) = rename startu (mkmodb mkbnull)
and startu = 1000
and i_ipreenv = rperm (rjoin (addmets (rlist Kmeth insts)) i_preenv)

and mker ne msg = mkap (mkerror msg) ne
and replcurmod modid allexps (mkexpidmodall i) & (eqid modid i) = allexps
||  replcurmod _ _ e = [e]
#if 0
and dbldefs nenv env = 
    if AllowRedef then [] 
    else map tl (rdbldefs Kvalue nenv env @ rdbldefs Ktype nenv env)
#endif

and mrename _ env u0 (mkmodule i fixs impl exp def) =
	    let rec (u1, defn, defs) = rendef dummyid (mkffv i fixs (map expid nexpl)) env u0 def
	    and     expl = case exp in Some es : es || None : xgetexps defs end
	    and     exps = map (idtostr o expid) expl
	    and     (errl, nexpl) = renexplist exps defs in
	    let ne = mkmodule i fixs [] (Some nexpl) defn in
	    (u1, defs, env,
	     (if null errl then
		ne
	     else -- we mustn't hide any errors, so make an appl.
	     	mkap (mkerror (concmap (\x.x@"\n") errl)) ne)
	    )
||  mrename auto oenvi u (mkhmodule id oexps imps fixs b) =
    let rec (u0, pienv, imperrs) = importe (chkfun oenvi) (idtostr id) u (rsetct nenvi cts) [] imps
    and     ienvi = runperm (addmets pienv)		-- imported environment
    and     cenvi = rjoin ienvi oenvi			-- old & imported
    and     nenvi = rjoin (rjoin denvi menvi) cenvi     -- new env, except supers = derived, module, imported, & old
--    and     xdefs = if RevVis then menv else rnil       -- module to interface defs
    and     (u1, b', menv) = rendef dummyid ff (rsetct cenvi cts) u0 (if NoToprec then b else mkbrec b)
    and     menvi = addmets menv
    and     (errl, nexpl) = hrenexplist (let allexps = xgetexps menv in 
					     case oexps in 
						Some es : concmap (replcurmod id allexps) es 
					     || None : allexps 
					     end) menvi
    and     mid = mkid u2 (idtostr id) (idi_module nexpl) noorigname
    and     nmod = mkmodule mid [] [] (Some nexpl) (mkband b' db)
    and     (css, circs) = ibuildsuper menv nenvi      -- only if module has class decls! SUPERS
    and     ff = mkffv id fixs (map expid nexpl)
    and     (eder, denvi, db, u2) = solvederiv auto ff menv ienvi oenvi u1
    and     (cts, instmsgs) = ibuildinsts css menvi nenvi
    and     jenvi = rjoin (rone Kmodule mid) (rjoin (rlist Kmeth (rids Kmeth menvi)) (rjoin denvi oenvi))
    in
(if X3 then trace (show_list show_Renv [ienvi; denvi; menvi]) else (\x.x))
--(if X1 then trace ("mrename menvi="@show_Renv menvi) else (\x.x))
	    (u2+1, rjoin ienvi menvi, rsetct jenvi cts,
	     if badmodname id then
		 mkerror ("[6] Illegal module name: "@oprid id)
	     else if imperrs ~= "" then
		 mkerror imperrs
	     else if eder ~= [] & null errl then
		 mkerror ("[7] Bad deriving for: "@mix eder ", ")
	     else if circs ~= [] then
		 mker nmod ("[9] Circular class structure "@mix circs ", ")
	     else if errl~=[] then
		 mker nmod (concmap (\x.x@"\n") errl)
             else if instmsgs ~= [] then
		 mkerror (mix instmsgs "\n")
	     else
		 nmod)

-- dupl from iload
and chkfun env s = eqid (rfind Ktype s env) dummyid
and runperm r = rjoin (rjoin (rjoin (rlist Kvalue (rids Kvalue r)) (rlist Ktype (rids Ktype r))) (rlist Kmodule (rids Kmodule r))) (rlist Kmeth (rids Kmeth r)) -- !!! runperm or fix rjoin
and gsupers env = let (_, _, sup) = rgetct env in sup

and icomp d env e0 autoexp mono u0 =
    let errmap = [] in
    let e02 = if Curry then curry0 e0 else lmlzf e0 in
    let (u1, nenv, xenv, e1) = mrename autoexp env u0 e02 in
    let (e1s, dflts) = remsign d e1 in
    let (e1q, u1q) = remzf e1s u1 in
    let e1c = remclass e1q in
    let (e2, u2) = constrtr e1c u1q in
    let (e2z, u2z) = remlazy e2 u2 in
    let (e3, u3) = remmatch e2z u2z in
    let errs = map (errors errmap) [e1; e1s; e1c; e2; e3] in
    let e4 = remdeep e3 in
    let E01 = Ecnv e4 in
    let E02 = if NoUnrec then E01 else unrec E01 in
    let E03 = if autoexp then
	case E02 in
	   Emodule i _ bs : Emodule i (xgetexps nenv) bs
	end
	else E02
    in
    let E0 = if mono then E03 else addovl E03 in
    let (terr, S, E1, _, u4) = if all null errs then tcheck errmap E0 dflts u3
			else ([], ifail "_", E0, ifail "_", u3) in
    let E11 = apconv E1 in
    let E12 = classtrans E11 in
    let E13 = predef E12 in
    let E14 = strictcon E13 in
    let E2 = conv E14 in
    let bs1 = Econv E2 in
(if X2 then trace (ppr e0@ppr e1@ppr e1s@ppr e1q@ppr e1c@ppr e2@ppr e2z@ppr e3@ppr e4@pr E02@pr E0@pr E11@pr E12@pr E13@pr E14@pr E2@
show_list (show_pair (prid,show_Cexpr)) bs1
) else (\x.x))
(
--trace (pr E12@show_list (show_pair (prid,show_Cexpr)) bs1) (
    let bs2 = mapsnd abstract bs1 in
    let emsg = getemsg (errs @ [terr]) in
    if null emsg then
        let venv = buildenv nenv E1 in
	Yes (bs2, venv, rjoin venv xenv, u4, dflts)
    else
	No emsg
)
and xgetexps env = concmap xexp (filter (\i.let c = hd (idtostr i) in c = '_' | c = 'P') (rids Kall env))
and xexp (i as mkid _ _ (idi_var _ _ _) _) = [mkexpid i]
||  xexp (i as mkid _ _ (idi_type _ _ _ _ _ _) _) = [mkexpidall i]
||  xexp (i as mkid _ _ (idi_view _ _ _) _) = [mkexpidall i]
||  xexp (i as mkid _ _ (idi_syn _ _ _ _) _) = [mkexpidall i]
||  xexp (i as mkid _ _ (idi_class _) _) = [mkexpidall i]
||  xexp _ = []

and     getemsg [] = []
||      getemsg ([].r) = getemsg r
||      getemsg (a._) = head 2000 (concmap (\e.e@"\n") a)
and conv e = (f e
where rec f (Emodule i expl ds) = Emodule i expl (map (mapsnd f) ds)
||        f (Ecase e ps d) = Ecase (f e) (mapthd f ps) (f d)
||        f (Elet false ds e) =	ap (lam (map fst ds) (f e)) (map (f o snd) ds)
||	  f (Elet true ds e) = f (lrec (mapsnd f ds) (f e))
||	  f (Econstr c es) = econstr c (map f es)
||	  f (Elam i e) = Elam i (f e)
||        f (Eap e1 e2) = Eap (f e1) (f e2)
||	  f (Eidapl i es) = ap (Evar i) (map f es)
||        f (Elaml is (Einfo vectordef e)) = Eap (Evar ivectorwrap) (f (Elaml is e))
||        f (Elaml is e) = lam is (f e)
||	  f (e as Evar _) = e
||	  f (e as Efailmatch _) = e
||	  f (e as Ecfunction _ _) = e
||	  f (Einfo _ e) = f e
)
and ivectorwrap = mkid Fvectorwrap "Pvectorwrap" idi_udef noorigname
and ap e xs = reduce (\x.\r.Eap r x) e (rev xs)
and lam xs e = reduce Elam e xs
and lrec [(i,x)] e = Eap (Elam i e) (Eap EY (Elam i x))
||  lrec ds e =
	let n = length ds in
	let is = map fst ds
	and es = map snd ds
	and t = mkid IDTMP "Pt" idi_udef noorigname
	and c = tup n
	in
        Ecase (Eap EY (Elam t (Econstr c (map (\e.Ecase (Evar t) [(c, is, e)] (Efailmatch 0)) es))))
	      [(c, is, e)] (Efailmatch 0)
and tup n = 
	let t = Ttuple n
	and na = tupstr n
	and tb = combine3 ((tvars n), (f where rec f=false.f), (f where rec f=None.f)) 
        and tb2 = combine ((tvars n), (f where rec f=false.f))
	in
	Cconstr na
		t
		(mktinfo t 1 false false [mkcons (ifail "tup") (false,[],[]) tb false] false false None)
		0 
                (false,[],[])
		tb2
and EY = Evar idY
and econstr c es = Econstr c es

-- build a new environment from the export list
and buildenv env (Emodule _ es _) = benv env [] [] es
and benv env vs ts [] = rjoin (rlist Kvalue vs) (rjoin (rlist Ktype ts) (rlist Kmeth (rids Kmeth env)))
||  benv env vs ts (mkexpid    (i as mkid _ _ (idi_var _ _ _) _)           . es) = benv env (i.vs)    ts     es
||  benv env vs ts (mkexpidall (i as mkid _ _ (idi_type _ _ _ t _ _) _)    . es) = benv env (cs t@vs) (i.ts) es
||  benv env vs ts (mkexpid    (i as mkid _ _ (idi_type _ _ _ t _ _) _)    . es) = if Curry then 
										   benv env vs     (ai i.ts) es else
									           benv env (cs t@vs) (i.ts) es
||  benv env vs ts (mkexpidall (i as mkid _ _ (idi_class c) _)             . es) = benv env (ms c@vs) (i.ts) es
||  benv env vs ts (mkexpidall (i as mkid _ _ (idi_syn _ _ _ _) _)         . es) = benv env vs        (i.ts) es
||  benv env vs ts (_                                                      . es) = benv env vs        ts     es
and cs ti = map (\(mkcons i _ _ _).i) (get_cs_from_tinfo ti)
and ai (mkid n s (idi_type k t m (mktinfo tt tm tn tf _ e iso vw) c d) r) = mkid n s (idi_type k t m (mktinfo tt tm tn tf [] e iso vw) c d) r
and ms (clsi _ _ dmts _ _ _) = map (\(_,m,_).m) dmts

-- add overloading to top level bindings
and addovl (Emodule is es bs) = Emodule is es (map (map add1ovl) bs)
and add1ovl (ie as (i,Elam _ _)) = ie
||  add1ovl (i, e) = (i, Einfo overload e)
end
