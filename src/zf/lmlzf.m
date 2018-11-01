module
#include "../expr/id_t.t"
#include "../expr/id.t"
#include "../expr/types_t.t"
#include "../expr/pprint.t"
#include "../syntax/listgen.h"
#include "../transform/misc.t"
export lmlzf;
rec
    icons = mkids "_."
and inil = mkids "_[]"
and itrue = mkids "_true"
and ifalse = mkids "_false"
and igt = mkids "_>"
and iadd = mkids "_+"
and isub = mkids "_-"
and ifrom = mkids "Pfrom"
and ifromby = mkids "Pfromby"
and ifromto = mkids "Pfromto"
and ifrombyto = mkids "Pfrombyto"
and c1 = mkconst (cint 1)
and elet i e1 e2 = mkletv (mkbrec (mkbpat [(i, e1)])) e2
and econs e1 e2 = mkap (mkap (mkident icons) e1) e2
and enil = mkident inil
and elam i e = mklam i e
and eif b t f = mkcase b [(mkident itrue, t); (mkident ifalse, f)]
and egt e1 e2 = mkap (mkap (mkident igt) e1) e2
and eap e1 e2 = mkap e1 e2
and einc e = mkap (mkap (mkident iadd) e) c1
and esub e1 e2 = mkap (mkap (mkident isub) e1) e2
and eadd e e1 e2 = mkap (mkap (mkident iadd) e) (esub e1 e2)
and edum = mkident (mkids "_")
--and newid n = mkident (mkids ("Lx"@itos n))
and chooseid i u = mkident (mkids ("LC" @ itos u @ idtostr i))

and
    L fid e [] t u =
	(econs e t, u)
||  L fid e (mkqfilter f.r) t u =
	let (q, u') = L fid e r t u
	in (eif f q t, u')
||  L fid e (mkqgen (x as mkident _) (mklistf L_FROM_TO [n; m]).r) t u =
	let g = chooseid fid u in
	let (q, u') = L fid e r (eap g (einc x)) (u+1)
	in (elet g (elam x (eif (egt x m) t q)) (eap g n), u')
||  L fid e (mkqgen (x as mkident _) (mklistf L_FROM [n]).r) t u =
	let g = chooseid fid u in
	let (q, u') = L fid e r (eap g (einc x)) (u+1)
	in (elet g (elam x q) (eap g n), u')
#if 0
WRONG IF k-n < 0
||  L fid e (mkqgen (x as mkident _) (mklistf L_FROM_BY_TO [n; k; m]).r) t u =
	let g = chooseid fid u in
	let (q, u') = L fid e r (eap g (eadd x k n)) (u+1)
	in (elet g (elam x (eif (egt x m) t q)) (eap g n), u')
#endif
||  L fid e (mkqgen (x as mkident _) (mklistf L_FROM_BY [n; k]).r) t u =
	let g = chooseid fid u in
	let (q, u') = L fid e r (eap g (eadd x k n)) (u+1)
	in (elet g (elam x q) (eap g n), u')
||  L fid e (mkqgen p l.r) t u =
	let g = chooseid fid u
	and xs = chooseid fid (u+1)
	in let (q, u') = L fid e r (eap g xs) (u+2)
	in (elet g (Lg fid g p t q xs) (eap g l), u')
and Lg fid g p t q xs =
	let a = chooseid fid 0 in
	let dum = (econs edum xs, eap g xs)
	and pat = econs p xs in
	let d =
		case p in
		   mkcondp _ _ : [(pat, q); dum]
		|| mkident _ : [(pat, q)]
		|| _ : [(mkcondp pat (mkident itrue), q); dum]
		end
	in
	mklam a (mkcase a ((enil, t) . d))

and mkif c t e = mkcase c [(mtrue, t); (mfalse, e)]
and mtrue = mkident (mkids "_true")
and mfalse = mkident (mkids "_false")
and trlex fid u (mklt c) = (u,mklt c)
||  trlex fid u (mkltint i) = (u,mkltint i)
||  trlex fid u (mkltid i) = (u,mkltid i)
||  trlex fid u (mkltsym i) = (u,mkltsym i)
||  trlex fid u (mkunq e) = 
	let (u',e') = tr fid u e in
	(u',mkunq e')
and
    tr fid u (mkap (mkap (mkap (mkident (mkids "Pif")) c) t) e) = tr fid u (mkif c t e)
||  tr fid u (mkap (mkap (mkident (mkids "_&")) x) y) = tr fid u (mkif x y mfalse)
||  tr fid u (mkap (mkap (mkident (mkids "_|")) x) y) = tr fid u (mkif x mtrue y)
||  tr fid u (mkap (mkident (mkids "_~")) x) = tr fid u (mkif x mfalse mtrue)

#if 1
||  tr fid u (mkap (mkap (mkident (mkids "_@")) (mklistg e gs)) r) =	-- Handle concatenated comprehensions better
	let (u', r') = tr fid u r in
        let (l, u'') = L fid e gs r' u' in
        tr fid u'' l
#endif
||  tr fid u (mkap e1 e2) =
	let (u', e1') = tr fid u e1 in
	let (u'', e2')= tr fid u' e2 in
	(u'', mkap e1' e2')
||  tr fid u (mklam e1 e2) =
	let (u', e1') = trp fid u e1 in
	let (u'', e2') = tr fid u e2 in
	(u'', mklam e1' e2')
||  tr fid u (mkcase e pes) =
	let (u', e') = tr fid u e in
	let (u'', pes') = mapstate (trpb fid) u pes in
	(u'', mkcase e' pes')
||  tr fid u (mkletv b e) =
	let (u', b') = trb fid u b in
	let (u'', e') = tr fid u' e in
	(u'', mkletv b' e')
||  tr fid u (e as mkident _) = (u, e)
||  tr fid u (mkmodule i ex im fi b) = 
	let (u', b') = trb fid u b in
	(u', mkmodule i ex im fi b')
||  tr fid u (e as mkconst _) = (u, e)
||  tr fid u (e as mkcfunction _ _) = (u, e)
||  tr fid u (mkbrack g llex) = 
	let (u',llex') = mapstate (trlex fid) u llex in
	(u',mkbrack g llex')
||  tr fid u (e as mkerror _) = (u, e)
/*
||  tr fid u (mkas i e) =
	let (u', e') = tr fid u e in
	(u', mkas i e')
||  tr fid u (mkcondp p e) =
	let (u', e') = tr fid u e in
	let (u'', p') = tr fid u' p in
	(u', mkcondp p' e')
*/
||  tr fid u (mkinfo t e) =
	let (u', e') = tr fid u e in
	(u', mkinfo t e')
||  tr fid u (mklistf L_FROM [e1]) =
	let (u', e1') = tr fid u e1 in
	(u', eap (mkident ifrom) e1')
||  tr fid u (mklistf L_FROM_TO [e1; e2]) =
	let (u', e1') = tr fid u e1 in
	let (u'', e2') = tr fid u' e2 in
	(u'', eap (eap (mkident ifromto) e1') e2')
||  tr fid u (mklistf L_FROM_BY [e1; e2]) =
	let (u', e1') = tr fid u e1 in
	let (u'', e2') = tr fid u' e2 in
	(u'', eap (eap (mkident ifromby) e1') (esub e2' e1'))
||  tr fid u (mklistf L_FROM_BY_TO [e1; e2; e3]) =
	let (u', e1') = tr fid u e1 in
	let (u'', e2') = tr fid u' e2 in
	let (u''', e3') = tr fid u'' e3 in
	(u''', eap (eap (eap (mkident ifrombyto) e1') (esub e2' e1')) e3')
||  tr fid u (mklistg e gs) =
	let (l, u') = L fid e gs enil u in
	tr fid u' l
||  tr fid u e = fail ("ZF-trans "@ppr e)
and
    trpb fid u (p, e) =
	let (u', e') = tr fid u e in
	let (u'', p') = trp fid u' p in
	(u'', (p', e'))
and
    trb fid u (b as mkbtype _ _ _ _) = (u, b)
||  trb fid u (b as mkbctype _ _) = (u, b)
||  trb fid u (mkbpat (pes as ((p,_)._))) =
	let ii = case leftmost p in
	            mkident i : i
	         ||    _      : fid 
	     end
	in
	let (u', pes') = mapstate (trpb ii) u pes in
	(u', mkbpat pes')
||  trb fid u (mkband b1 b2) =
	let (u', b1') = trb fid u b1 in
	let (u'', b2')= trb fid u' b2 in
	(u'', mkband b1' b2')
||  trb fid u (mkbrec b) =
	let (u', b') = trb fid u b in
	(u', mkbrec b')
||  trb fid u (mkblocal b1 b2) =
	let (u', b1') = trb fid u b1 in
	let (u'', b2')= trb fid u' b2 in
	(u'', mkblocal b1' b2')
||  trb fid u (b as mkbnull) = (u, b)
||  trb fid u (b as mkbsyn _ _) = (u, b)
||  trb fid u (b as mkbsign _ _) = (u, b)
and trp fid u (mkcondp p e) =
    let (u', e') = tr fid u e in
    (u', mkcondp p e')
||  trp fid u p = (u, p)
and lmlzf e = snd (tr dummyid 1 e)
end
