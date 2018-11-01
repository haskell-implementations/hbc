module -- remzf
#include "../expr/types_t.t"
#include "../expr/id_t.t"
#include "../expr/id.t"
#include "../expr/pprint.t"
#include "../expr/tinfo.t"
#include "../expr/einfo_t.t"
#include "../expr/constrfun.t"
#include "../misc/misc.t"
#include "../misc/util.t"
#include "../transform/hexpr.t"
#include "../transform/exprs.t"
#include "../transform/misc.t"
#include "../transform/cutil.t"
#include "../transform/lettrans.t" /* andify */
#include "../misc/flags.t"
export remzf;
rec remzf e u = if Curry then rz dummyid e u else (e, u)
and rz fid e u =
	case e in
           mkap (mkap (mkident cnc) (mklistg e qs)) r & (eqid cnc hiconc) :
		let (r', u') = rz fid r u in
		let (e', u'') = L fid e qs r' u' in
		rz fid e' u''
	|| mkap f a :
		let (f1, u1) = rz fid f u in
		let (a1, u2) = rz fid a u1 in
		(mkap f1 a1, u2)
	|| mklam i e :	Uap (\e1.mklam i e1) (rz fid e u)
	|| mkcase e pl :
		let (e1, u1) = rz fid e u in
		let (pl1, u2) = Umap (\(p, e).\u.
			let (np, u1) = rzp fid p u in
			let (ne, u2) = rz fid e u1 in
			(np, ne),u2) pl u1 in
		(mkcase e1 (fixstmtfail pl1), u2)
	|| mkletv b e :
		let (b1, u1) = rzb fid b u in
		let (e1, u2) = rz fid e u1 in
		(mkletv b1 e1, u2)
	|| mkident _ : (e, u)
	|| mkmodule i fs is es b : Uap (mkmodule i fs is es) (rzb fid b u)
	|| mkerror _ : (e, u)
	|| mkconstr c el : Uap (mkconstr c) (Umap (rz fid) el u)
	|| mkconst _ : (e, u)
	|| mkcfunction _ _ : (e, u)
	|| mkinfo t e : Uap (mkinfo t) (rz fid e u)
	|| mklistg e qs :
		let (e', u') = L fid e qs enil u in
		rz fid e' u'
#if 1
	|| mkwhere ges b : 
	        let (ges', u') = Umap (\(g,e).\u.let (g', u') = rz fid g u in 
                                                 let (e', u'') = rz fid e u' in ((g', e'), u'')) ges u in
                let (b', u'') = rzb fid b u' in
	        (mkwhere ges' b', u'')
#endif
	|| (e as (mkrecord _ _ _)) : 
	        let (u', e') = doupdate u e in
		rz fid e' u'
	|| e : fail ("No match in rz: "@ppr e)
	end
and
    rzb fid b u =
	case b in
	   mkband b1 b2 :
		let (b11, u1) = rzb fid b1 u in
		let (b21, u2) = rzb fid b2 u1 in
		(mkband b11 b21, u2)
	|| mkbrec b : Uap mkbrec (rzb fid b u)
        || mkbmulti p e :
	       let (p', u') = rzp fid p u in
	       let (e', u'') = rz fid e u' in
	       (mkbmulti p' e', u'')
	||  (mkbpat (pes as ((p,e)._))) :
        	let ii = case (leftmost p,e) in
                            (mkident i, mklam _ _) : i
                         || (mkident i, _) & (eqid fid dummyid) : i
                 	 ||    _      : fid
                         end
        	in
	       let (pes', u') = Umap (\(p, e).\u.
			let (np, u1) = rzp ii p u in
			let (ne, u2) = rz ii e u1 in
			(np, ne),u2) pes u
	       in (mkbpat pes', u')
	|| mkberror _ : (b, u)
	|| mkbnull : (b, u)
	|| mkblocal b1 b2 :
		let (b11, u1) = rzb fid b1 u in
		let (b21, u2) = rzb fid b2 u1 in
		(mkblocal b11 b21, u2)
	|| mkbtype _ _ _ _ : (b, u)
        || mkbview t ot cs b : Uap (mkbview t ot cs) (rzb fid b u)
	|| mkbpragma _ : (b, u)
	|| mkbclass t b : Uap (mkbclass t) (rzb fid b u)
	|| mkbinstance t b oi : Uap (\b.mkbinstance t b oi) (rzb fid b u)
	|| mkbsign _ _ : (b, u)
	|| mkbdefault _ : (b, u)
        || _ : fail ("rzb "@prdefg 0 b)
	end
and rzp fid (mkcondp p c) u =
	let (nc, u1) = rz fid c u in (mkcondp p nc, u1)
||  rzp fid p u = (p, u)

and doupdate u (mkrecord c ies cs) =
        let (is, es) = split ies in
	let nes = map2 (\ e . \ u . (mknewid "R" u, e)) es (from u) in
	let tbl = combine (is, map fst nes) in
	let mkpat (mkcons c _ tbis _) u =
	    let xs = for u (u+length tbis-1) (mknewid "r")
	    and mkrhs x (_,_,Some s) = mkident (assocdefeq eqid s tbl x)
	    in ((mkapl (mkident c) (map mkident xs),
		 mkapl (mkident c) (map2 mkrhs xs tbis)),
		   u+length tbis)
	in
	let (pes, u1) = Umap mkpat cs (u+length ies) in
	(u1, mkletv (andify (map ( \ (i,e) . mkbpat [(mkident i,e)]) nes))
	            (mkcase c pes))
and mkapl s es = reduce (\a.\f.mkap f a) s (reverse es)

and eif b t f = mkcase b [(mkident hitrue, t); (mkident hifalse, f)]
and econs e1 e2 = mkap (mkap (mkident hicons) e1) e2
and enil = mkident hinil
and elet i e1 e2 = mkletv (mkbrec (mkbpat [(i, e1)])) e2
and eap e1 e2 = mkap e1 e2
and eap2 e1 e2 e3 = mkap (eap e1 e2) e3
and eap3 e1 e2 e3 e4 = mkap (eap2 e1 e2 e3) e4
and eap4 e1 e2 e3 e4 e5 = mkap (eap3 e1 e2 e3 e4) e5
and edum = mkident dummyid
and etrue = mkident hitrue
and canfail (mkident _) = false
||  canfail (mkas _ p) = canfail p
||  canfail _ = true
and goodpat (mkident (mkid _ _ (idi_constr _ _ _ _ _ _ _) _)) = false
||  goodpat _ = true
and
    L fid e [] t u =
	(econs e t, u)
||  L fid e (mkqfilter f.r) t u =
	let (q, u') = L fid e r t u
	in (eif f q t, u')
||  L fid e (mkqlet b.r) t u =
	let (e', u') = L fid e r t u in
	(mkletv b e', u')
-- It seems to be an idiom to write [ x | p<-[e]], so handle that specially
||  L fid e (mkqgen p (mkap (mkap (mkident ic) es) (mkident ini)).r) t u & (eqid ic hicons & eqid ini hinil & goodpat p) =
	let (e', u') = L fid e r t u in
        case p in
	    mkident _ : (mkletv (mkbpat [(p,es)]) e', u')
        ||  _ : (mkcase es ((p, e'). (if canfail p then [(edum, t)] else [])), u')
        end
-- handle (x,y)<-zip xs ys
||  L fid e (mkqgen (mkap (mkap (mkident ip) (mkident x)) (mkident y)) (mkap (mkap (mkident iz) xs0) ys0).r) t u & (Optimize & eqid ip hipair & eqid iz hizip) =
	let g = newids (idtostr fid) u
	and xs = newids (idtostr fid) (u+1)
	and ys = newids (idtostr fid) (u+2)
	in let (q, u') = L fid e r (eap2 g xs ys) (u+3)
	in (elet g (Lg2 fid g [mkident x; mkident y] t q [xs;ys] u') (eap2 g xs0 ys0), u'+2)
||  L fid e (mkqgen (mkap (mkap (mkap (mkident ip) (mkident x)) (mkident y)) (mkident z)) (mkap (mkap (mkap (mkident iz) xs0) ys0) zs0).r) t u & (Optimize & eqid ip hit3 & eqid iz hizip3) =
	let g = newids (idtostr fid) u
	and xs = newids (idtostr fid) (u+1)
	and ys = newids (idtostr fid) (u+2)
	and zs = newids (idtostr fid) (u+3)
	in let (q, u') = L fid e r (eap3 g xs ys zs) (u+4)
	in (elet g (Lg2 fid g [mkident x; mkident y; mkident z] t q [xs;ys;zs] u') (eap3 g xs0 ys0 zs0), u'+3)
||  L fid e (mkqgen (mkap (mkap (mkap (mkap (mkident ip) (mkident x)) (mkident y)) (mkident z)) (mkident w)) (mkap (mkap (mkap (mkap (mkident iz) xs0) ys0) zs0) ws0).r) t u & (Optimize & eqid ip hit4 & eqid iz hizip4) =
	let g = newids (idtostr fid) u
	and xs = newids (idtostr fid) (u+1)
	and ys = newids (idtostr fid) (u+2)
	and zs = newids (idtostr fid) (u+3)
	and ws = newids (idtostr fid) (u+4)
	in let (q, u') = L fid e r (eap4 g xs ys zs ws) (u+5)
	in (elet g (Lg2 fid g [mkident x; mkident y; mkident z; mkident w] t q [xs;ys;zs;ws] u') (eap4 g xs0 ys0 zs0 ws0), u'+4)
||  L fid e (mkqgen p l.r) t u =
	let g = newids (idtostr fid) u
	and xs = newids (idtostr fid) (u+1)
	in let (q, u') = L fid e r (eap g xs) (u+2)
	in (elet g (Lg fid g p t q xs u') (eap g l), u'+1)

and Lg fid g p t q xs u =
	let a = newids (idtostr fid) u in
	let dum = (econs edum xs, eap g xs)
	and pat = econs p xs in
	let d =
	        if canfail p then
		    [(pat, q); dum]
		else
		    [(pat, q)]
	in
	mklam a (mkcase a ((enil, t) . d))

and Lg2 fid g ps t q xss u =
	let args = map (newids (idtostr fid)) [u..u+length ps-1] in
	let pats = map2 econs ps xss in
	mklams args (mkcase (mtuple args) [(mtuple pats, q); (edum, t)])
and mklams [] e = e
||  mklams (x.xs) e = mklam x (mklams xs e)
and mtuple l = revitlist (\a.\f.mkap f a) l (mkident (tups ?? (length l-2)))
and tups = [hipair; hit3; hit4]

-----

and fixstmtfail [] = []
||  fixstmtfail ((p, mkinfo (stmtfailpoint e1) e2) . xs) =
        if isfailurefree e1 then
            fixstmtfail xs
        else
            (p, e2) . fixstmtfail xs
||  fixstmtfail (x . xs) = x . fixstmtfail xs

and isfailurefree (mkident (i as mkid _ _ (idi_constr _ _ _ _ _ _ _) _)) = 
    get_no_of_constr_from_tinfo (ctinfo (idtoconstr i)) = 1
||  isfailurefree (mkident _) = true
||  isfailurefree (mklazyp _) = true
||  isfailurefree (mkap p1 p2) = isfailurefree p1 & isfailurefree p2
||  isfailurefree (mkinfo _ p) = isfailurefree p
||  isfailurefree _ = false

end
