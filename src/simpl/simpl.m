module -- simpl
#include "../funnos.h"
#include "../expr/id_t.t"
#include "../expr/constr_t.t"
#include "../expr/constrfun.t"
#include "../expr/id.t"
#include "../expr/einfo_t.t"
#include "../ExprE/Expr_t.t"
#include "../ExprE/Eutil.t"
#include "../ExprE/Eprint.t"
#include "../ExprE/Esubst.t"
#include "../ExprE/Egetid.t"
#include "../ExprE/apconv.t"
#include "../misc/misc.t"
#include "../misc/util.t"
#include "../misc/flags.t"
#include "../transform/hexpr.t"
#include "casetr.t"
#include "asimpl.t"
#include "mlet.t"
#include "sutil.t"

-- The reference counting used to determine if substitution should be
-- performed is a kludge.  It should be redone, maybe using attributes.

export Esimpl, simpl, apply;
rec
-- noinnercase checks if the default expressions is reachable by a jump.
-- (could be better!)
    noinnercase _ (Efailmatch 0) = true
||  noinnercase es _ = (all ncase es
    where
	ncase (_,_,(Ecase _ _ _)) = false
    ||  ncase _ = true
    )
and iscase (Ecase _ _ _) = true
||  iscase _ = false
and
    constfold d Fadd [e1;e2] = Emkint(e1 + e2)
||  constfold d Fsub [e1;e2] = Emkint(e1 - e2)
||  constfold d Fmul [e1;e2] = Emkint(e1 * e2)
||  constfold d Fdiv [e1;e2] & (e2 ~= 0) = Emkint(e1 / e2)
||  constfold d Fmod [e1;e2] & (e2 ~= 0) = Emkint(e1 % e2)
||  constfold d Fneg [e1] = Emkint(-e1)
||  constfold d Ford [e1] = Emkint(e1)
||  constfold d Fchr [e1] = Emkchar(chr e1)
||  constfold d Feq [e1;e2] = Emkbool(e1  = e2)
||  constfold d Fne [e1;e2] = Emkbool(e1 ~= e2)
||  constfold d Flt [e1;e2] = Emkbool(e1 <  e2)
||  constfold d Fle [e1;e2] = Emkbool(e1 <= e2)
||  constfold d Fgt [e1;e2] = Emkbool(e1 >  e2)
||  constfold d Fge [e1;e2] = Emkbool(e1 >= e2)
||  constfold d m   _ & (mem m [Fbigeq; Fbigne; Fbigle; Fbigge; Fbiglt; Fbiggt; Fstreq; Fstrne; Fstrle; Fstrge; Fstrlt; Fstrgt]) =
        let (Eidapl _ [e1; e2]) = d in
	case bcmp e1 e2 in
	   Unknown : d
	|| Lt : Emkbool (mem m [Fbiglt; Fbigle; Fbigne; Fstrlt; Fstrle; Fstrne])
	|| Gt : Emkbool (mem m [Fbiggt; Fbigge; Fbigne; Fstrgt; Fstrge; Fstrne])
        || Eq : Emkbool (mem m [Fbigeq; Fbigle; Fbigge; Fstreq; Fstrle; Fstrge])
        end
||  constfold (d as Eidapl _ (es as ((Econstr c _)._))) op _ & ((issfloat c | isdfloat c) & FloatInstr) = 
        dconstfold d op (map getfloat es)
||  constfold d _ _ = d

and getfloat (Econstr c _) = stof (cname c)
and Edfloat f = Econstr (mkdfloat (fmtf ".17e" f)) []
and Esfloat f = Econstr (mksfloat (fmtf ".9e" f)) []

and dconstfold d Fdfadd [e1;e2] = Edfloat(e1 +. e2)
||  dconstfold d Fdfsub [e1;e2] = Edfloat(e1 -. e2)
||  dconstfold d Fdfmul [e1;e2] = Edfloat(e1 *. e2)
||  dconstfold d Fdfdiv [e1;e2] & (e2 ~= 0.0) = Edfloat(e1 /. e2)
||  dconstfold d Fdfneg [e1] = Edfloat(-.e1)
||  dconstfold d Fdfeq [e1;e2] = Emkbool(e1  = e2)
||  dconstfold d Fdfne [e1;e2] = Emkbool(e1 ~= e2)
||  dconstfold d Fdflt [e1;e2] = Emkbool(e1 <  e2)
||  dconstfold d Fdfle [e1;e2] = Emkbool(e1 <= e2)
||  dconstfold d Fdfgt [e1;e2] = Emkbool(e1 >  e2)
||  dconstfold d Fdfge [e1;e2] = Emkbool(e1 >= e2)
||  dconstfold d Fsfadd [e1;e2] = Esfloat(e1 +. e2)
||  dconstfold d Fsfsub [e1;e2] = Esfloat(e1 -. e2)
||  dconstfold d Fsfmul [e1;e2] = Esfloat(e1 *. e2)
||  dconstfold d Fsfdiv [e1;e2] & (e2 ~= 0.0) = Esfloat(e1 /. e2)
||  dconstfold d Fsfneg [e1] = Esfloat(-.e1)
||  dconstfold d Fsfeq [e1;e2] = Emkbool(e1  = e2)
||  dconstfold d Fsfne [e1;e2] = Emkbool(e1 ~= e2)
||  dconstfold d Fsflt [e1;e2] = Emkbool(e1 <  e2)
||  dconstfold d Fsfle [e1;e2] = Emkbool(e1 <= e2)
||  dconstfold d Fsfgt [e1;e2] = Emkbool(e1 >  e2)
||  dconstfold d Fsfge [e1;e2] = Emkbool(e1 >= e2)
||  dconstfold d _    _       = d

and type Rel = Unknown + Lt + Gt + Eq
and rand Eq y = y
||  rand x  y = x
and bcmp (Econstr c1 l1) (Econstr c2 l2) & (cno c1 >= 0 & cno c2 >= 0) = 
             if cno c1 < cno c2 then Lt
        else if cno c1 > cno c2 then Gt
        else reduce rand Eq (map2 bcmp l1 l2)
||  bcmp _ _ = Unknown

and casefold u d (Econstr c es) cies de =
	assocdef (cval c)
		(map (\(c,is,e).(cval c,
			if e = Efailmatch 1 then
				(u, de)
			else
				mlet u d (combine (is, es)) e)) cies)
		(u, de)

-- Make an alpha-converted copy of an expression to avoid name clashes
and clone u (e as Elaml is _) =
    let clonef = (\(i as mkid _ a b on).\n.(i, mkid n a b on)) in
    let iis = map2 clonef is (from u) in
    (u+length is, Ealphasubst iis e)

and nocafs is (Eidapl i es) = member eqid i is | all (nocafs is) es
||  nocafs is (Econstr _ es) = all (nocafs is) es
||  nocafs is (Einfo _ e) = nocafs is e
||  nocafs is _ = false
and smax n = 10*Optlevel+1+2*n			       -- some arbitrary limit for function inlining
-- this will inline even recursive functions once, but so what?
and issimplfun (Elaml is e) = (InlineFun & size e < smax (length is) & nocafs is e		-- TURN THIS OFF!??
    where rec size (Eidapl _ es) = 1 + 2 * Sum (map size es)
	   || size (Econstr _ es) = 1 + Sum (map size es)
           || size (Einfo _ e) = size e
	   || size _ = 10000)
||  issimplfun _ = false
and issimpl (Eidapl _ []) = true
||  issimpl (Econstr _ []) = true
||  issimpl (Einfo inline _) = true
||  issimpl (e as Elaml _ _) = issimplfun e
||  issimpl e = false
--and getsimpl (ie as [(i, e)]) & (issimpl e) = ie
--||  getsimpl _ = []

-- Should not keep defn's that are expanded.
and Esimpl u (Emodule i e iess) =
    let (iess', (uu',_)) = (Umap (Umap f) iess (u,[])
        where f (i, e) (u,sdef) =
	    let (u', e') = topsimpl u sdef e in
            let sdef' = if issimpl e' then (i, e').sdef else sdef in
	    ((i, e'), (u',sdef')))
    in (uu', Emodule i e iess')

and apply [] e = e
||  apply es e = (A e
    where rec
    A (ec as Ecase e cies de) = 
        Ecase e (mapthd A cies) (A de)
||  A (Elet r ds e) = Elet r ds (A e)
||  A (oe as Eidapl i [_]) & (isidfail i) = oe		-- Pfail only takes 1 argument no matter what!
||  A (Eidapl i es1) = Eidapl i (es1@es)
||  A (Elaml is e) = transap is e es
||  A (Efailmatch n) = Efailmatch n
||  A (Einfo i e) = Einfo i (A e)
||  A e = fail ("apply:"@pr e)
)

and topsimpl u d (Elaml is (Elaml is' e)) = topsimpl u d (Elaml (is@is') e)
||  topsimpl u d (Elaml is e) = let (u', e') = simpl u d e in (u', Elaml is e')
||  topsimpl u d e = simpl u d e

and simpl u d e =
/*
trace (	"simpl "@
	show_list (show_pair(pprid,pr)) d@"\n"@
	pr e @ "==>\n" @ pr (snd r) @ "\n") r
where r =
*/
    (S u e
    where rec
    S u (Ecase e cies de) =
	let (u', ne) = S u e in
	if CaseOpt & iscase ne then
		S u (casetr (Ecase ne cies de))
	else
	let (u'', ncies) = (mapstate f u' cies
	    where f u (c, is, e) = (let (u', e') = S u e in
				    let v = refc (xgetid e') in
				    (u', (c, map g is, e'))
		where g i = if assocdefeq eqid i v 0 > 0 then i else dummyid))
	in
	let (u''', nde) = S u'' de in
	if isc ne & noinnercase ncies de then
		casefold u''' d ne ncies nde
	else
		(u''', Ecase ne ncies nde)
||  S u (Elet r ies e) =
	if ~r then mlet u d ies e
	else mletr u d ies e
||  S u (Econstr c es) = 
	let (u', es') = mapstate S u es
	in (u', Econstr c es')
||  S u (Eidapl i es) & (id_is_predef i)=
-- All predefined are strict so the case optim is safe
	let (u', nes) = mapstate S u es in
	if CaseOpt & exists iscase nes then
		S u (casetr (Eidapl i nes))
	else
	if all isc nes then 
		(u', constfold (Eidapl i nes) (id_no i) (map value nes))
	else
		(u', asimpl (Eidapl i nes))
||  S u (Eidapl i es) =
	let (u', es') = mapstate S u es in
	case assocdefeq eqid i d (Eidapl i []) in
	    Eidapl i' [] : (u', Eidapl i' es')
        ||  (e as Elaml is _) : 
	     if length is <= length es then
		 let (u'', Elaml is e) = clone u' e in
		 -- remove i to avoid infinite unfolding
                 simpl u'' (filter (\ (i1,_).not (eqid i i1)) d) (apply (tail (length is) es') (Elet false (combine(is,es')) e))
	     else
		 (u', Eidapl i es')     -- avoid inserting lambda expressions
        ||  e : (u', concstr (apply es' e))
        end
||  S u (Einfo i e) =
	let (u', e') = S u e in
	(u', Einfo i e')
||  S u (Efailmatch n) = (u, Efailmatch n)
||  S u (e as Ecfunction _ _) = (u, e)
||  S u e = fail ("simpl: "@pr e)
)
end

