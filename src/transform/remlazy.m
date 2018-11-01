module -- remlazy.m
--
-- remove ~ and n+k patterns
-- Insert fromInteger, fromRational and a type restriction on Main.main
-- Function composition is also expanded to lambda (it's done here instead of in
-- predef because we have unique numbers here and doing it before typecheck speeds
-- that up).
--
#include "../expr/types_t.t"
#include "../expr/pprint.t"
#include "../expr/id_t.t"
#include "../expr/id.t"
#include "../expr/constrfun.t"
#include "../expr/einfo_t.t"
#include "../expr/ttype_t.t"
#include "../expr/ttype.t"
#include "../misc/misc.t"
#include "../misc/flags.t"
#include "../rename/multi.t"
#include "lettrans.t"
#include "misc.t"
#include "case.t"
#include "hexpr.t"
#include "exprs.t"

export remlazy;
rec
    rlz e u =
	case e in
	   mkap (mkap (mkident idot) f) g & (eqid idot hicomp) :
--	       let (es, u1) = Umap rlz (f . collectcomp g) (u+1)
--	       and ni = newid u in
--	       (mklam ni (reduce mkap ni es), u1)
	       let es = f . collectcomp g
	       and ni = newid u in
	       rlz (mklam ni (reduce mkap ni es)) (u+1)
	|| mkap (mkap (mkident idol) f) a & (eqid idol hidol) :
		rlz (xmkap f a) u
	|| mkap (mkap (mkap (mkident iflip) f) x) y & (eqid iflip hiflip) :
	        rlz (mkap (mkap f y) x) u
	|| mkap f a :
		let (f1, u1) = rlz f u in
		let (a1, u2) = rlz a u1 in
		(mkap f1 a1, u2)
	|| mklam i e :	Uap (mklam i) (rlz e u)
	|| mkcase e pl :
		let (e1, u1) = rlz e u in
		let (pl1, u2) = Umap (\(p, e).\u.
			let (npe, u1) = elimz p e u in
			(npe, u1)) pl u1 in
		(mkcase e1 pl1, u2)
	|| mkletv b e :
		let (b1, u1) = rlzb b u in
		let (e1, u2) = rlz e u1 in
		(mkletv b1 e1, u2)
	|| mkident _ : (e, u)
	|| mkmodule i fs is es b : Uap (mkmodule i fs is es) (rlzb b u)
	|| mkerror _ : (e, u)
	|| mkcfunction _ _ : (e, u)
        || mkconstr c [] & (isinteger c | isrational c) : (ccnv e, u)
	|| mkconstr c el : Uap (mkconstr c) (Umap rlz el u)
	|| mkinfo t e : Uap (mkinfo t) (rlz e u)
	|| mkwhere ges b : 
	        let (ges', u') = Umap (\(g,e).\u.let (g', u') = rlz g u in 
                                                 let (e', u'') = rlz e u' in ((g', e'), u'')) ges u in
                let (b', u'') = rlzb b u' in
	        (mkwhere ges' b', u'')
        || e : fail ("No match in rlz: "@ppr e)
	-- mkconst, mkas, mkcondp, mkfailmatch cannot occur
	end
and xmkap (mklam (mkident i) (mkconstr c [mkident i'])) e & (eqid i i') =
	mkconstr c [e]
||  xmkap f a = mkap f a
and rlzp (mkcondp p e) u = 
    let (e', u') = rlz e u in
    uppz [e'] p u'
||  rlzp p u = uppz [] p u
and uppz cs p u =
        let (gbs, p', u') = trz p u in
	let (gs, bs) = split gbs in
	let rgs = filter (not o istrue) gs @ cs in
	let xs = filter (not o isbnull) bs in
        (mkcp p' rgs, xs, u')
and
    rlzb b u =
	case b in
	   mkband b1 b2 :
		let (b11, u1) = rlzb b1 u in
		let (b21, u2) = rlzb b2 u1 in
		(mkband b11 b21, u2)
	|| mkbrec b : Uap mkbrec (rlzb b u)
	|| mkbmulti p e :
	        let (p1, bs, u1) = rlzp p u in
		let (e1, u2) = rlz e u1 in
		(mkca (mkbmulti p1 e1) bs, u2)
	|| mkbpat [(i, e)] : Uap (\e1.mkbpat [insres i e1]) (rlz e u)
	|| mkberror _ : (b, u)
	|| mkbnull : (b, u)
	|| mkblocal b1 b2 :
		let (b11, u1) = rlzb b1 u in
		let (b21, u2) = rlzb b2 u1 in
		(mkblocal b11 b21, u2)
	|| (b as (mkbtype _ _ _ _)) : (b, u)
	|| (b as (mkbpragma _)) : (b, u)
	end
and elimz (mkcondp p e) x u = 
    let (e', u') = rlz e u in
    upz [e'] p x u'
||  elimz p x u = upz [] p x u
and upz cs p x u =
        let (gbs, p', u') = trz p u in
	let (gs, bs) = split gbs in
	let rgs = filter (not o istrue) gs @ cs in
	let xs = filter (not o isbnull) bs in
	let (x', u'') = rlz (mlet xs x) u' in
	((mkcp p' rgs, x'), u'')
and mlet [] x = x
||  mlet bs (mkwhere ges b) = mkwhere ges (mkband b (andify bs))
||  mlet bs x = mkletv (andify bs) x
and mkcp p [] = p
||  mkcp p gs = mkcondp p (andthem gs)
and andthem [g] = g
||  andthem (g.gs) = mkap2 eand g (andthem gs)
and isbnull mkbnull = true
||  isbnull _ = false
and istrue (mkconstr c []) = cno c = 1
||  istrue _ = false
and restrIntegral = restr [] (mktcontype [mkassert hiIntegral [0]] (mktvar 0))
and trz (mkap (mkap (mkident i) (n as mkident _)) (k as mkconstr _ [])) u & (NPlusK & eqid i hiadd) =
        let ni = newid u in
	([(mkap2 ege ni (mkinfo restrIntegral (mkap efi k)), mkbpat [(n,mkap2 esub ni k)])], ni, u+1)
||  trz (mkap f a) u = 
	let (gbs1, f1, u1) = trz f u in
	let (gbs2, a1, u2) = trz a u1 in
	(gbs1@gbs2, mkap f1 a1, u2)
||  trz (e as mkident _) u = ([], e, u)
||  trz (e as mkconstr c []) u & (isrational c | isinteger c) =
	let ni = newid u in
	([(mkap2 eeq ni (ccnv e), mkbnull)], ni, u+1)
||  trz (mkconstr c el) u =
	let ((gbs, u1), el1) = 
	         mapstate (\(gbs1, u).\p.
	                 let (gbs2, c, u') = trz p u in
			 ((gbs1@gbs2, u'), c)) ([], u) el
	in (gbs, mkconstr c el1, u1)
||  trz (mklazyp (p as mklazyp _)) u = trz p u
||  trz (mklazyp (e as mkident _)) u = ([], e, u)
||  trz (mklazyp p) u =
        let ni = newid u in
        let (b, u') = rlzb (mkbmulti p ni) (u+1) in
	([(etrue, b)], ni, u')
||  trz (mkas i p) u =
        let (gbs, p', u') = trz p u in
	(gbs, mkas i p', u')
||  trz p u = ([], p, u)
and mkca b [] = b
||  mkca b bs = mkbrec (mkband b (andify bs))
and mkap2 f a1 a2 = mkap (mkap f a1) a2
and eand = mkident hiand
and esub = mkident hisub
and ege  = mkident hige
and eeq  = mkident hieq
and etrue= mkconstr hctrue []
and efi  = mkident hifromInteger
and efr  = mkident hifromRational
and insres i e & (ismain i) = 
    if H1_3 then
	(i, mkinfo (restr [] (mktcons hiIO [mktcons hiUnit []])) e)
    else
	(i, mkinfo (restr [] (mktcons hiDialogue [])) e)
||  insres i e = (i, e)
and ismain (mkident i) = id_isvar i & (if FlatNames then idtostr i = "_main" else id_orignames i = (MI "_Main", "_main"))
||  ismain _ = false
and ccnv (e as mkconstr c _) = if isinteger c then mkap efi e else mkap efr e

and collectcomp (mkap (mkap (mkident idot) f) g) & (eqid idot hicomp) = f . collectcomp g
||  collectcomp e = [e]

and remlazy e u = if Curry then rlz e u else (e, u)
end

