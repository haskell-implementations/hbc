module -- predef
-- Insert predefined identifiers where possible.
#include "../expr/id_t.t"
#include "../expr/id.t"
#include "../expr/einfo_t.t"
#include "../expr/constr_t.t"
#include "../expr/ttype.t"
#include "../expr/booltree.t"
#include "../ExprE/Expr_t.t"
#include "../transform/hexpr.t"
#include "../misc/util.t"
#include "../misc/misc.t"
#include "../misc/flags.t"
#include "../rename/renenv.t"
#include "../rename/renameutil.t" /* mkcompound */
#include "../main/topexprs.t"
#include "../funnos.h"
#include "../expr/constrfun.t"
#include "../transform/exprs.t"
#include "Eutil.t"
export predef, pre_add, pre_sub, pre_neg, bigeqops, bigordops;
rec predef e = lpre e

and lpre (Ecase e1 cl e2) = xEcase (lpre e1) (mapthd lpre cl) (lpre e2)
||  lpre (Elet r dl e) = Elet r (mapsnd lpre dl) (lpre e)
||  lpre (Emodule i expl dl) = Emodule i expl (map (mapsnd lpre) dl)
||  lpre (Econstr c []) & (isrational c) = torat c
||  lpre (e as Ecfunction _ _) = e
||  lpre (Econstr (cc as Cconstr s a b _ _ c) _) & (isstring cc) = -- translate short strings into lists
	case length s in
	   0 : Econstr hcnil []
	|| 1 : Econstr hccons [Econstr (xcchar (hd s)) [] ; Econstr hcnil []]
	|| _ : Econstr hccons [Econstr (xcchar (hd s)) [] ; Econstr (Cconstr (tl s) a b (-1) (false,[],[]) c) [] ]
	end
||  lpre (Econstr c el) = Econstr c (map lpre el)
||  lpre (e as Efailmatch n) = e
-- Next 4 are for Haskell only
||  lpre (Eidapl iand [e1; e2]) & (Curry & eqid iand hiand) = 
	Ecase (lpre e1) [(hcfalse, [], Econstr hcfalse []); (hctrue, [], (lpre e2))] (Efailmatch 0)
||  lpre (Eidapl ior  [e1; e2]) & (Curry & eqid ior  hior)  = 
	Ecase (lpre e1) [(hcfalse, [], (lpre e2)); (hctrue, [], Econstr hctrue  [])] (Efailmatch 0)
||  lpre (Eidapl inot [e]) & (Curry & eqid inot hinot) = 
	Ecase (lpre e) [(hcfalse, [], Econstr hctrue []); (hctrue, [], Econstr hcfalse  [])] (Efailmatch 0)
||  lpre (Eidapl iother []) & (Curry & eqid iother hiotherwise) = Econstr hctrue []
||  lpre (Eidapl i0 (Econstr _ [Eidapl i1 []; _] . es)) & (CompCurry & member eqid i0 rfops & eqid i1 viint) =
    if eqid i0 mdtrunc then
	lchk (Eidapl pdftoi (map lpre es))
    else if eqid i0 mstrunc then
	lchk (Eidapl psftoi (map lpre es))
    else
	Eidapl (assocdefeq eqid i0 rfiops (fail "rfiops")) (map lpre es)
||  lpre (Eidapl i1 es) & (Curry & idtostr i1 = "_(Int,Int)->[Assoc Int a]->Array Int a.array") =
	Eidapl iintarray (map lpre es)
||  lpre (Eidapl i1 es) & (Curry & idtostr i1 = "_(Array Int a)->Int->a.!") =
#if 0
	if Optimize & length es = 2 then
	    -- expand indexing operation inline
	    -- intIndex (MkArray (l,u) a) i =
	    --         if i < l || i > u then
	    --             outside
	    --         else
	    --             rindex a (i-l)
            -- let i = e2
	    -- let m = e1 in
	    -- case m in
	    --    (lu,a) : case lu` in
	    --               (l,u) : if i < l` || i > u` then Poutside else Pxindex a (i-l`)
	    let i_i = mknewid "ii" 770
	    and i_m = mknewid "im" 771
	    and i_lu =mknewid "ilu" 772
	    and i_l = mknewid "il" 773
	    and i_u = mknewid "iu" 774
	    and i_a = mknewid "ia" 775 in
	    let e_i = Eidapl i_i []
	    and e_l = Einfo noeval (Eidapl i_l []) in
	    let cnd = lpre (Eidapl hior [Eidapl pre_lt [e_i; e_l];
					 Eidapl pre_gt [e_i; Einfo noeval (Eidapl i_u [])]]) in
	    let expr = 	Elet false (i_m,lpre (hd es)) (
			Elet false (i_i,lpre (hd (tl es))) (
			Ecase (Eidapl i_m []) [(hcpair, [i_lu; i_a], 
				Ecase (Einfo noeval (Eidapl i_lu [])) [(hcpair, [i_l;i_u],
					     (Ecase cnd [(hcfalse, [], Eidapl ioutside []);
							 (hctrue,  [], Eidapl iindex [Eidapl i_a []; Eidapl isub [e_i; e_l]])
							]
					            (Efailmatch 0))
								      )]
				      (Efailmatch 0)
                                              )]
			      (Efailmatch 0)))
	    in  ...
	else
#endif
            Eidapl iintindex (map lpre es)
||  lpre (Eidapl i1 [e]) & (member eqid i1 idops) = lpre e
||  lpre (Eidapl i1 [Eidapl i2 es]) & (CompCurry & eqid i1 mdfr & eqid i2 mftr) = lchk (Eidapl psftodf es)
||  lpre (Eidapl i1 [Eidapl i2 es]) & (CompCurry & eqid i1 mffr & eqid i2 mdtr) = lchk (Eidapl pdftosf es)
||  lpre (Eidapl i1 (Econstr _ [Eidapl vi [];_]. Econstr _ [Eidapl vd [];_].es)) & (CompCurry & eqid i1 ifrI & eqid vi viint & eqid vd vndbl) =
        lchk (Eidapl pitodf (map lpre es))
||  lpre (Eidapl i1 (Econstr _ [Eidapl vi [];_]. Econstr _ [Eidapl vd [];_].es)) & (CompCurry & eqid i1 ifrI & eqid vi viint & eqid vd vnflt) =
        lchk (Eidapl pitosf (map lpre es))
||  lpre (Eidapl i1 [e]) & (CompCurry & ~H1_3 & eqid i1 minteven) = -- XXX 1.3
	Eidapl pre_and [Eidapl pre_add [lpre e; Emkint 1]; Emkint 1]
||  lpre (Eidapl i1 [e]) & (CompCurry & ~H1_3 & eqid i1 mintodd) = -- XXX 1.3
	Eidapl pre_and [lpre e; Emkint 1]
||  lpre (Eidapl i1 [e1;Econstr c _]) & (CompCurry & eqid i1 mintmul & cno c > 0 & elog2 (cno c) ~= None) =
        Eidapl pre_lsh  [lpre e1; Emkint (unsome (elog2 (cno c)))]
||  lpre (Eidapl i1 [e1;Econstr c _]) & (CompCurry & eqid i1 mintdiv & cno c > 0 & elog2 (cno c) ~= None) =
        Eidapl pre_rsha [lpre e1; Emkint (unsome (elog2 (cno c)))]
||  lpre (Eidapl i1 [e1;Econstr c _]) & (CompCurry & eqid i1 mintmod & cno c > 0 & elog2 (cno c) ~= None) =
        Eidapl pre_and [lpre e1; Emkint (cno c - 1)]
||  lpre (Eidapl i1 [e1;Econstr c _]) & (CompCurry & cno c = 2 & sinexp i1) =
	Eidapl pre_isquare [lpre e1]
||  lpre (Eidapl i1 [e1;Econstr c _]) & (CompCurry & cno c = 2 & ssfnexp i1) =
	Eidapl pre_sfsquare [lpre e1]
||  lpre (Eidapl i1 [e1;Econstr c _]) & (CompCurry & cno c = 2 & sdfnexp i1) =
	Eidapl pre_dfsquare [lpre e1]
||  lpre (e as Eidapl i []) = assocdefeq eqid i contab e
||  lpre (Eidapl i (es as (_._))) = 
    let es' = map lpre es in
    if Interactive then Eidapl i es' else
    case assocdefeq eqid i pretab (prechk i) in
	(ni as mkid _ _ (idi_var (var_pre (finfo n _ _ _ _)) _ _) _) & (length es = n) : Eidapl ni es'
    ||  (ni as mkid _ _ (idi_var (var_global _) _ _) _) & (length es = arity_of_id ni) : Eidapl ni es'
    ||  _ : Eidapl i es'
    end
||  lpre (Eidapl i es) = Eidapl i (map lpre es)
||  lpre (Elaml is e) = Elaml is (lpre e)
||  lpre (Einfo f e) = Einfo f (lpre e)

and CompCurry = Curry & ~Interactive

-- a hack!
and lchk (Eidapl i []) = 
        let l = mknewid "ll" 878
	in  Elaml [l] (Eidapl i [Eidapl l []])
||  lchk e = e

and prechk (i as mkid _ (s as ('P'._)) _ _) = idlook i s predefs
||  prechk i = i
and idlook i _ [] = i
||  idlook i s ((p as mkid _ ps _ _).r) = if s=ps then p else idlook i s r

 -- turn nested Peq tests back into a case
and xEcase (e as (Eidapl (mkid Feq _ _ _)
		         [Eidapl i [];
		          Econstr c _]))
           cl
           (ed as (Efailmatch 0)) =
 case cl in
    [(_,_, Ecase (ei as (Eidapl i' [])) cies ed); (_,_,etrue)] & (eqid i i') : 
    Ecase ei (ciesort ((c, [], etrue) . cies)) ed
 || _ :
    let (kes, ef) = collecteqs [] i (Ecase e cl ed) in
    if length kes > 2 then
	let cies = ciesort kes
	in  Ecase (Eidapl i []) cies ef
    else
	Ecase e cl ed
 end
||  xEcase e cl ed = Ecase e cl ed

and collecteqs kes i 
            (Ecase
               (Eidapl (mkid Feq _ _ _)
		       [Eidapl i' _;
			Econstr c _])
               [(_, _, efalse); (_, _, etrue)] _) & (eqid i i') =
        collecteqs ((c, [], etrue) . kes) i efalse
||  collecteqs kes _ ef = (kes, ef)

and ciesort cies = sort (\ (x, _, _) . \ (y, _, _) . cno x < cno y) cies

and pretab = if Curry then hpretab else lpretab
and lpretab = [
(hiadd, pf Fadd);
(hisub, pf Fsub);
(himul, pf Fmul);
(hidiv, pf Fdiv);
(himod, pf Fmod);
(hinegate, pf Fneg);
(hi_ord, pf Ford);
(hi_chr, pf Fchr);
(hiseq, pf Fseq);
(hiband, pf Fand);
(hibor, pf For);
(hibxor, pf Fxor);
(hibcompl, pf Fcompl);
(hiblsh, pf Flsh);
(hibrsh, pf Frsh);
(hibrsha, pf Frsha);
(vf "Ptag", pf Ftag);
(vf "Pcno", pf Fcno);
(vf "Prindex", pf Frindex);
(vf "Prindexu", pf Frindexu)
]@(if ~FloatInstr then [
(hidfadd, vf "DFloatAdd");
(hidfsub, vf "DFloatSub");
(hidfmul, vf "DFloatMul");
(hidfdiv, vf "DFloatDiv")
#if 0
;(hisfadd, vf "SFloatAdd");
(hisfsub, vf "SFloatSub");
(hisfmul, vf "SFloatMul");
(hisfdiv, vf "SFloatDiv")
#endif
] else [
(hidfadd, pf Fdfadd);
(hidfsub, pf Fdfsub);
(hidfmul, pf Fdfmul);
(hidfdiv, pf Fdfdiv);
(hidfnegate, pf Fdfneg);
(hiitodf, pf Fitodf);
(hidftoi, pf Fdftoi)
#if 0
(hisfadd, pf Fsfadd);
(hisfsub, pf Fsfsub);
(hisfmul, pf Fsfmul);
(hisfdiv, pf Fsfdiv);
(hisfnegate, pf Fsfneg);
(hiitosf, pf Fitosf);
(hisftoi, pf Fsfftoi)
#endif
])
and pf n = pff n predefs
and pff n [] = fail ("No predef "@itos n)
||  pff n ((i as mkid k _ _ _)._) & (n = k) = i
||  pff n (_.xs) = pff n xs
and v n = btors [btvar n]
and v01 = btands [v 0; v 1]
and v0 = btands [v 0]
and pre2  = idi_var (var_pre (finfo 2 [] (v01, v01)  2 None)) Onotype None
and pre2f = idi_var (var_pre (finfo 2 [] (v01, btff) 2 None)) Onotype None
and pre2s = idi_var (var_pre (finfo 2 [] (bttt,btff) 2 None)) Onotype None
and pre1  = idi_var (var_pre (finfo 1 [] (v0,  v0)   1 None)) Onotype None
and nn = noorigname
and predefs = [
mkid Fadd   "Padd"   pre2  nn;
mkid Fsub   "Psub"   pre2  nn;
mkid Fmul   "Pmul"   pre2  nn;
mkid Fdiv   "Pdiv"   pre2f nn;
mkid Fmod   "Pmod"   pre2f nn;
mkid Fneg   "Pneg"   pre1  nn;
mkid Ford   "Pord"   pre1  nn;
mkid Fchr   "Pchr"   pre1  nn;
mkid Fseq   "Pseq"   pre2f nn;
mkid Feq    "Peq"    pre2  nn;
mkid Fne    "Pne"    pre2  nn;
mkid Flt    "Plt"    pre2  nn;
mkid Fle    "Ple"    pre2  nn;
mkid Fgt    "Pgt"    pre2  nn;
mkid Fge    "Pge"    pre2  nn;
mkid Fcno   "Pcno"   pre1  nn;
mkid Ftag   "Ptag"   pre1  nn;
mkid Fand   "Pand"   pre2  nn;
mkid For    "Por"    pre2  nn;
mkid Fxor   "Pxor"   pre2  nn;
mkid Fcompl "Pcompl" pre1  nn;
mkid Flsh   "Plsh"   pre2  nn;
mkid Frsh   "Prsh"   pre2  nn;
mkid Frsha  "Prsha"  pre2  nn;
mkid Fsquare "Psquare" pre1 nn;
mkid Fsfsquare "Psfsquare" pre1 nn;
mkid Fdfsquare "Pdfsquare" pre1 nn;

mkid Fdfadd  "Pdfadd"   pre2  nn;
mkid Fdfsub  "Pdfsub"   pre2  nn;
mkid Fdfmul  "Pdfmul"   pre2  nn;
mkid Fdfdiv  "Pdfdiv"   pre2f nn;
mkid Fdfmod  "Pdfmod"   pre2f nn;
mkid Fdfneg  "Pdfneg"   pre1  nn;
mkid Fdftoi  "Pdftoi"   pre1  nn;
mkid Fdftosf "Pdftosf"  pre1  nn;
mkid Fitodf  "Pitodf"   pre1  nn;
mkid Fdfeq   "Pdfeq"    pre2  nn;
mkid Fdfne   "Pdfne"    pre2  nn;
mkid Fdflt   "Pdflt"    pre2  nn;
mkid Fdfle   "Pdfle"    pre2  nn;
mkid Fdfgt   "Pdfgt"    pre2  nn;
mkid Fdfge   "Pdfge"    pre2  nn;

mkid Fsfadd  "Psfadd"   pre2  nn;
mkid Fsfsub  "Psfsub"   pre2  nn;
mkid Fsfmul  "Psfmul"   pre2  nn;
mkid Fsfdiv  "Psfdiv"   pre2f nn;
mkid Fsfmod  "Psfmod"   pre2f nn;
mkid Fsfneg  "Psfneg"   pre1  nn;
mkid Fsftoi  "Psftoi"   pre1  nn;
mkid Fsftodf "Psftodf"  pre1  nn;
mkid Fitosf  "Pitosf"   pre1  nn;
mkid Fsfeq   "Psfeq"    pre2  nn;
mkid Fsfne   "Psfne"    pre2  nn;
mkid Fsflt   "Psflt"    pre2  nn;
mkid Fsfle   "Psfle"    pre2  nn;
mkid Fsfgt   "Psfgt"    pre2  nn;
mkid Fsfge   "Psfge"    pre2  nn;

mkid Frindex "Prindex"  pre2f nn;
mkid Frindexu "Prindexu" pre2f nn
]@bigeqops@bigordops
and bigeqops = [	-- order of these is important for selection via Eq/Ord
mkid Fbigeq "Pbigeq" pre2f nn;
mkid Fbigne "Pbigne" pre2f nn
]
and bigordops = [
mkid Fbiglt "Pbiglt" pre2f nn;
mkid Fbigle "Pbigle" pre2f nn;
mkid Fbigge "Pbigge" pre2f nn;
mkid Fbiggt "Pbiggt" pre2f nn
]
and pre_add = pf Fadd
and pre_sub = pf Fsub
and pre_neg = pf Fneg
and pre_and = pf Fand
and pre_rsha= pf Frsha
and pre_lsh = pf Flsh
and pre_isquare = pf Fsquare
and pre_sfsquare = pf Fsfsquare
and pre_dfsquare = pf Fdfsquare

and xxxxx i s = if id_no i = 0 & Curry then /*fail*/ trace ("predef-predef "@s) i else i --!!!
-- Haskell predefined
and vf s = xxxxx (rfind Kvalue (mkps s) preenv) s
and mf s = xxxxx (rfind Kmeth  (mkps s) preenv) s
and mkps (s as 'M'.'M'._) = s
||  mkps (s as 'V'.'V'._) = s
||  mkps s = case mkpids s in mkids s : s end
and hpretab = [
-- all builtin ops
(mf (mkcompound ["MM"; mkcompprel "Num"; mkcompprel "Int"; "+"]),	pf Fadd);
(mf (mkcompound ["MM"; mkcompprel "Num"; mkcompprel "Int"; "-"]), 	pf Fsub);
(mf (mkcompound ["MM"; mkcompprel "Num"; mkcompprel "Int"; "*"]), 	pf Fmul);
(mf (mkcompound ["MM"; mkcompprel "Num"; mkcompprel "Int"; "negate"]),pf Fneg);
(mf (mkcompound ["MM"; mkcompprel "Integral"; mkcompprel "Int"; "quot"]), pf Fdiv);
(mf (mkcompound ["MM"; mkcompprel "Integral"; mkcompprel "Int"; "rem"]), pf Fmod);
(mf (mkcompound ["MM"; mkcompprel "Eq"; mkcompprel "Int"; "=="]), 	pf Feq);
(mf (mkcompound ["MM"; mkcompprel "Eq"; mkcompprel "Int"; "/="]), 	pf Fne);
(mf (mkcompound ["MM"; mkcompprel "Ord"; mkcompprel "Int"; ">"]), 	pf Fgt);
(mf (mkcompound ["MM"; mkcompprel "Ord"; mkcompprel "Int"; ">="]), 	pf Fge);
(mf (mkcompound ["MM"; mkcompprel "Ord"; mkcompprel "Int"; "<"]), 	pf Flt);
(mf (mkcompound ["MM"; mkcompprel "Ord"; mkcompprel "Int"; "<="]), 	pf Fle);
(mf (mkcompound ["MM"; mkcompprel "Eq"; mkcompprel "Char"; "=="]), 	pf Feq);
(mf (mkcompound ["MM"; mkcompprel "Eq"; mkcompprel "Char"; "/="]), 	pf Fne);
(mf (mkcompound ["MM"; mkcompprel "Ord"; mkcompprel "Char"; ">"]), 	pf Fgt);
(mf (mkcompound ["MM"; mkcompprel "Ord"; mkcompprel "Char"; ">="]), 	pf Fge);
(mf (mkcompound ["MM"; mkcompprel "Ord"; mkcompprel "Char"; "<"]), 	pf Flt);
(mf (mkcompound ["MM"; mkcompprel "Ord"; mkcompprel "Char"; "<="]), 	pf Fle);
(mf (mkcompound ["MM"; mkcompprel "Eq"; mkcompprel "Bool"; "=="]), 	pf Feq);
(mf (mkcompound ["MM"; mkcompprel "Eq"; mkcompprel "Bool"; "/="]), 	pf Fne);
(mf (mkcompound ["MM"; mkcompprel "Ord"; mkcompprel "Bool"; ">"]), 	pf Fgt);
(mf (mkcompound ["MM"; mkcompprel "Ord"; mkcompprel "Bool"; ">="]), 	pf Fge);
(mf (mkcompound ["MM"; mkcompprel "Ord"; mkcompprel "Bool"; "<"]), 	pf Flt);
(mf (mkcompound ["MM"; mkcompprel "Ord"; mkcompprel "Bool"; "<="]), 	pf Fle);
(hi_ord,		pf Ford);
(hi_chr,		pf Fchr);
(hicno,			pf Fcno);
(hieqint,		pf Feq);
(hiltint,		pf Flt);
(hileint,		pf Fle);
(hiord,			pf Ford);
(hichr,			pf Ftag);
(vf "Prindex", 		pf Frindex);
(vf "Prindexu",		pf Frindexu);
(vf "Pgeneq",		pf Fbigeq);
(vf "Pgenne",		pf Fbigne);
(vf "Pgengt",		pf Fbiggt);
(vf "Pgenge",		pf Fbigge);
(vf "Pgenlt",		pf Fbiglt);
(vf "Pgenle",		pf Fbigle);
-- faster calls to special routines
(mf (mkcompound ["MM"; mkcompprel "Num"; mkcompprel "Int"; "fromInteger"]), vf "PInteger2Int");
(mf (mkcompound ["MM"; mkcompprel "Integral"; mkcompprel "Int"; "toInteger"]), vf "PInt2Integer");
(mf (mkcompound ["MM"; mkcompprel "Eq"; mkcompprel "Integer"; "=="]),	pf Fbigeq);
(mf (mkcompound ["MM"; mkcompprel "Eq"; mkcompprel "Integer"; "/="]),	pf Fbigne);
(mf (mkcompound ["MM"; mkcompprel "Ord"; mkcompprel "Integer"; ">"]), pf Fbiggt);
(mf (mkcompound ["MM"; mkcompprel "Ord"; mkcompprel "Integer"; ">="]),pf Fbigge);
(mf (mkcompound ["MM"; mkcompprel "Ord"; mkcompprel "Integer"; "<"]), pf Fbiglt);
(mf (mkcompound ["MM"; mkcompprel "Ord"; mkcompprel "Integer"; "<="]),pf Fbigle);
(mf (mkcompound ["MM"; mkcompprel "Num"; mkcompprel "Integer"; "+"]),	vf "PIntegerAdd");
(mf (mkcompound ["MM"; mkcompprel "Num"; mkcompprel "Integer"; "-"]), vf "PIntegerSub");
(mf (mkcompound ["MM"; mkcompprel "Num"; mkcompprel "Integer"; "*"]), vf "PIntegerMul");
(mf (mkcompound ["MM"; mkcompprel "Num"; mkcompprel "Integer"; "negate"]),vf "PIntegerNeg");
(mf (mkcompound ["MM"; mkcompprel "Integral"; mkcompprel "Integer"; "quot"]), vf "PIntegerDiv");
(mf (mkcompound ["MM"; mkcompprel "Integral"; mkcompprel "Integer"; "rem"]), vf "PIntegerMod");
(mf (mkcompound ["MM"; mkcompprel "Integral"; mkcompprel "Integer"; "quotRem"]), vf "PIntegerDivMod")] @
dblops @
[
#if 0
(mf (mkcompound ["MM"; mkcompprel "Num"; mkcompprel "Double"; "abs"]),  vf "Dfabs")
;
(mf (mkcompound ["MM"; mkcompprel "Floating"; mkcompprel "Double"; "exp"]), vf "Dexp");
(mf (mkcompound ["MM"; mkcompprel "Floating"; mkcompprel "Double"; "log"]), vf "Dlog");
(mf (mkcompound ["MM"; mkcompprel "Floating"; mkcompprel "Double"; "sqrt"]), vf "Dsqrt");
(mf (mkcompound ["MM"; mkcompprel "Floating"; mkcompprel "Double"; "sin"]), vf "Dsin");
(mf (mkcompound ["MM"; mkcompprel "Floating"; mkcompprel "Double"; "cos"]), vf "Dcos");
(mf (mkcompound ["MM"; mkcompprel "Floating"; mkcompprel "Double"; "tan"]), vf "Dtan");
(mf (mkcompound ["MM"; mkcompprel "Floating"; mkcompprel "Double"; "asin"]), vf "Dasin");
(mf (mkcompound ["MM"; mkcompprel "Floating"; mkcompprel "Double"; "acos"]), vf "Dacos");
(mf (mkcompound ["MM"; mkcompprel "Floating"; mkcompprel "Double"; "atan"]), vf "Datan");
(mf (mkcompound ["MM"; mkcompprel "Floating"; mkcompprel "Double"; "sinh"]), vf "Dsinh");
(mf (mkcompound ["MM"; mkcompprel "Floating"; mkcompprel "Double"; "cosh"]), vf "Dcosh");
(mf (mkcompound ["MM"; mkcompprel "Floating"; mkcompprel "Double"; "tanh"]), vf "Dtanh")
#endif
] @
if H1_3 then
[
(mf (mkcompound ["MM"; mkcompprel "Enum"; mkcompprel "Char"; "fromEnum"]), pf Ford);
(mf (mkcompound ["MM"; mkcompprel "Enum"; mkcompprel "Char"; "toEnum"]), pf Fchr)
]
else
[
]


and Edpi = Econstr (mkdfloat "3.141592653589793238") []
and Espi = Econstr (mksfloat "3.141592653589793238") []

and contab = if Curry & FloatInstr then
[
(mf (mkcompound ["MM"; mkcompprel "Floating"; mkcompprel "Float"; "pi"]), Espi);
(mf (mkcompound ["MM"; mkcompprel "Floating"; mkcompprel "Double"; "pi"]), Edpi)
]
else
[]


and dblops =
if FloatInstr then
[
(mf (mkcompound ["MM"; mkcompprel "Eq"; mkcompprel "Float"; "=="]),	pf Fsfeq);
(mf (mkcompound ["MM"; mkcompprel "Eq"; mkcompprel "Float"; "/="]),	pf Fsfne);
(mf (mkcompound ["MM"; mkcompprel "Ord"; mkcompprel "Float"; ">"]),   pf Fsfgt);
(mf (mkcompound ["MM"; mkcompprel "Ord"; mkcompprel "Float"; ">="]),  pf Fsfge);
(mf (mkcompound ["MM"; mkcompprel "Ord"; mkcompprel "Float"; "<"]),   pf Fsflt);
(mf (mkcompound ["MM"; mkcompprel "Ord"; mkcompprel "Float"; "<="]),  pf Fsfle);
(mf (mkcompound ["MM"; mkcompprel "Num"; mkcompprel "Float"; "+"]),	pf Fsfadd);
(mf (mkcompound ["MM"; mkcompprel "Num"; mkcompprel "Float"; "-"]),   pf Fsfsub);
(mf (mkcompound ["MM"; mkcompprel "Num"; mkcompprel "Float"; "*"]),   pf Fsfmul);
(mf (mkcompound ["MM"; mkcompprel "Num"; mkcompprel "Float"; "negate"]),pf Fsfneg);
(mf (mkcompound ["MM"; mkcompprel "Num"; mkcompprel "Float"; "fromInt"]),pf Fitosf);
(mf (mkcompound ["MM"; mkcompprel "Fractional"; mkcompprel "Float"; "/"]), pf Fsfdiv);
(mf (mkcompound ["MM"; mkcompprel "Eq"; mkcompprel "Double"; "=="]),	pf Fdfeq);
(mf (mkcompound ["MM"; mkcompprel "Eq"; mkcompprel "Double"; "/="]),	pf Fdfne);
(mf (mkcompound ["MM"; mkcompprel "Ord"; mkcompprel "Double"; ">"]),  pf Fdfgt);
(mf (mkcompound ["MM"; mkcompprel "Ord"; mkcompprel "Double"; ">="]), pf Fdfge);
(mf (mkcompound ["MM"; mkcompprel "Ord"; mkcompprel "Double"; "<"]),  pf Fdflt);
(mf (mkcompound ["MM"; mkcompprel "Ord"; mkcompprel "Double"; "<="]), pf Fdfle);
(mf (mkcompound ["MM"; mkcompprel "Num"; mkcompprel "Double"; "+"]),	pf Fdfadd);
(mf (mkcompound ["MM"; mkcompprel "Num"; mkcompprel "Double"; "-"]),  pf Fdfsub);
(mf (mkcompound ["MM"; mkcompprel "Num"; mkcompprel "Double"; "*"]),  pf Fdfmul);
(mf (mkcompound ["MM"; mkcompprel "Num"; mkcompprel "Double"; "negate"]),pf Fdfneg);
(mf (mkcompound ["MM"; mkcompprel "Num"; mkcompprel "Double"; "fromInt"]),pf Fitodf);
(mf (mkcompound ["MM"; mkcompprel "Fractional"; mkcompprel "Double"; "/"]), pf Fdfdiv);
(vf "PFloat2Double", pf Fsftodf);
(vf "PDouble2Float", pf Fdftosf)
]
else
[
(mf (mkcompound ["MM"; mkcompprel "Eq"; mkcompprel "Float"; "=="]),	pf Fbigeq);
(mf (mkcompound ["MM"; mkcompprel "Eq"; mkcompprel "Float"; "/="]),	pf Fbigne);
(mf (mkcompound ["MM"; mkcompprel "Ord"; mkcompprel "Float"; ">"]),   pf Fbiggt);
(mf (mkcompound ["MM"; mkcompprel "Ord"; mkcompprel "Float"; ">="]),  pf Fbigge);
(mf (mkcompound ["MM"; mkcompprel "Ord"; mkcompprel "Float"; "<"]),   pf Fbiglt);
(mf (mkcompound ["MM"; mkcompprel "Ord"; mkcompprel "Float"; "<="]),  pf Fbigle);
(mf (mkcompound ["MM"; mkcompprel "Num"; mkcompprel "Float"; "+"]),	vf "SFloatAdd");
(mf (mkcompound ["MM"; mkcompprel "Num"; mkcompprel "Float"; "-"]),   vf "SFloatSub");
(mf (mkcompound ["MM"; mkcompprel "Num"; mkcompprel "Float"; "*"]),   vf "SFloatMul");
(mf (mkcompound ["MM"; mkcompprel "Num"; mkcompprel "Float"; "negate"]),vf "SFloatNeg");
(mf (mkcompound ["MM"; mkcompprel "Num"; mkcompprel "Float"; "fromInt"]),vf "Sitof");
(mf (mkcompound ["MM"; mkcompprel "Fractional"; mkcompprel "Float"; "/"]), vf "SFloatDiv");
(mf (mkcompound ["MM"; mkcompprel "Eq"; mkcompprel "Double"; "=="]),	pf Fbigeq);
(mf (mkcompound ["MM"; mkcompprel "Eq"; mkcompprel "Double"; "/="]),	pf Fbigne);
(mf (mkcompound ["MM"; mkcompprel "Ord"; mkcompprel "Double"; ">"]),  pf Fbiggt);
(mf (mkcompound ["MM"; mkcompprel "Ord"; mkcompprel "Double"; ">="]), pf Fbigge);
(mf (mkcompound ["MM"; mkcompprel "Ord"; mkcompprel "Double"; "<"]),  pf Fbiglt);
(mf (mkcompound ["MM"; mkcompprel "Ord"; mkcompprel "Double"; "<="]), pf Fbigle);
(mf (mkcompound ["MM"; mkcompprel "Num"; mkcompprel "Double"; "+"]),	vf "DFloatAdd");
(mf (mkcompound ["MM"; mkcompprel "Num"; mkcompprel "Double"; "-"]),  vf "DFloatSub");
(mf (mkcompound ["MM"; mkcompprel "Num"; mkcompprel "Double"; "*"]),  vf "DFloatMul");
(mf (mkcompound ["MM"; mkcompprel "Num"; mkcompprel "Double"; "negate"]),vf "DFloatNeg");
(mf (mkcompound ["MM"; mkcompprel "Num"; mkcompprel "Double"; "fromInt"]),vf "Ditof");
(mf (mkcompound ["MM"; mkcompprel "Fractional"; mkcompprel "Double"; "/"]), vf "DFloatDiv")
]
and sinexp  i = idtostr i = "_Int->Int->Int'^"
and ssfnexp i = idtostr i = "_Float->Int->Float'^"
and sdfnexp i = idtostr i = "_Double->Int->Double'^"

and mintodd = mf (mkcompound ["MM"; mkcompprel "Integral"; mkcompprel "Int"; "odd"])
and minteven= mf (mkcompound ["MM"; mkcompprel "Integral"; mkcompprel "Int"; "even"])
and mintmul = mf (mkcompound ["MM"; mkcompprel "Num"; mkcompprel "Int"; "*"])
and mintdiv = mf (mkcompound ["MM"; mkcompprel "Integral"; mkcompprel "Int"; "div"])
and mintmod = mf (mkcompound ["MM"; mkcompprel "Integral"; mkcompprel "Int"; "mod"])
and mdtrunc = mf (mkcompound ["MM"; mkcompprel "RealFrac"; mkcompprel "Double"; "truncate"])
and mdceil  = mf (mkcompound ["MM"; mkcompprel "RealFrac"; mkcompprel "Double"; "ceiling"])
and mdfloor = mf (mkcompound ["MM"; mkcompprel "RealFrac"; mkcompprel "Double"; "floor"])
and mdround = mf (mkcompound ["MM"; mkcompprel "RealFrac"; mkcompprel "Double"; "round"])
and viint   = mf (mkcompound ["VV"; mkcompprel "Integral"; mkcompprel "Int"])
and vndbl   = mf (mkcompound ["VV"; mkcompprel "Num"; mkcompprel "Double"])
and vnflt   = mf (mkcompound ["VV"; mkcompprel "Num"; mkcompprel "Float"])
and pdftoi  = pf Fdftoi
and mstrunc = mf (mkcompound ["MM"; mkcompprel "RealFrac"; mkcompprel "Float"; "truncate"])
and msceil  = mf (mkcompound ["MM"; mkcompprel "RealFrac"; mkcompprel "Float"; "ceiling"])
and msfloor = mf (mkcompound ["MM"; mkcompprel "RealFrac"; mkcompprel "Float"; "floor"])
and msround = mf (mkcompound ["MM"; mkcompprel "RealFrac"; mkcompprel "Float"; "round"])
and psftoi  = pf Fsftoi
and ifrI    = vf "_fromIntegral"
and pitodf  = pf Fitodf
and pitosf  = pf Fitosf
and rfops   = map fst rfiops
and rfiops = [(mdtrunc, fail "mdtrunc"); 
	      (mdceil, dceil);
	      (mdfloor, dfloor);
	      (mstrunc, fail "mstrunc");
	      (msceil, sceil);
	      (msfloor, sfloor)
	    /*;(mdround, dround);
	      (msround, sround)*/]  -- rounding not implemented in RF_Spec'hs yet
and dceil  = vf "PceilingDouble2Int"
and dfloor = vf "PfloorDouble2Int"
and dround = vf "ProundDouble2Int"
and sceil  = vf "PceilingFloat2Int"
and sfloor = vf "PfloorFloat2Int"
and sround = vf "ProundFloat2Int"

and mdfr = mf (mkcompound ["MM"; mkcompprel "Fractional"; mkcompprel "Double"; "fromRational"])
and mffr = mf (mkcompound ["MM"; mkcompprel "Fractional"; mkcompprel "Float"; "fromRational"])
and mdtr = mf (mkcompound ["MM"; mkcompprel "Real"; mkcompprel "Double"; "toRational"])
and mftr = mf (mkcompound ["MM"; mkcompprel "Real"; mkcompprel "Float"; "toRational"])
and psftodf = pf Fsftodf
and pdftosf = pf Fdftosf

and IfromI = mf (mkcompound ["MM"; mkcompprel "Num"; mkcompprel "Integer"; "fromInteger"])
and ifromi = mf (mkcompound ["MM"; mkcompprel "Num"; mkcompprel "Int"; "fromInt"])
and ItoI   = mf (mkcompound ["MM"; mkcompprel "Integral"; mkcompprel "Integer"; "toInteger"])
and itoi   = mf (mkcompound ["MM"; mkcompprel "Integral"; mkcompprel "Int"; "toInt"])
and idops  = [IfromI; ifromi; ItoI; itoi]

and iintarray = vf "PintArray"
and iintindex = vf "PintIndex"
and iindex = pf Frindex

-- Exact log2 if it exists
and elog2 n = if bitand n (n-1) = 0 then Some (fbit n) else None
and fbit n = if bitand n 1 = 1 then 0 else 1+fbit (bitrsh n 1)
and unsome (Some n) = n

#ifdef NOINTEGER
and stoiI s = fail "stoiI"
and Iitos s = fail "Iitos"
#endif
-- Convert a floating point string to a rational number.
-- Assumes that rationals are represented as a pair of Integer.
and torat c =
    let x = cname c in
    let (i,de) = splitat '.' x in
    let (d, e) = take isdigit de in
    let num1 = stoiI (i@d)
    and den1 = tenI (length d) in
    let exp = case e in
	          "" : 0
	      ||  _.'+'.s : stoi s	-- skipped char must be 'e' or 'E'
	      ||  _.s : stoi s
	      end in
    let (num2, den2) = if exp < 0 then 
	                 (num1, den1 *# tenI (-exp)) 
		       else 
			 (num1 *# tenI exp, den1) in
    let g = gcdI (absI num2) den2 in
    let den = den2 /# g
    and num = num2 /# g in
    Econstr crat [mkI num; mkI den]
and mkI n = Econstr (mkinteger (Iitos n)) []
and crat = Cconstr "_:%" Trational (mktinfo Trational 1 false false [] false false None) 0 (false,[],[]) [(Tarr Tinteger (Tarr Tinteger Trational), false)]

and absI n = if n < 0# then 0# -# n else n
and gcdI n m = if m = 0# then
                  n
	      else
		  gcdI m (n %# m)
and tenI n = 
        if n = 0 then
	    1#
        else
	    let p = tenI (n / 2) in
            if n % 2 = 0 then
		p *# p
	    else
		10# *# p *# p
end
