module -- genderiv
#include "../misc/triple.t"
#include "../expr/types_t.t"
#include "../expr/id.t"
#include "../expr/id_t.t"
#include "../expr/ttype.t"
#include "../expr/ttype_t.t"
#include "../expr/pprint.t"
#include "../expr/einfo_t.t"
#include "../misc/misc.t"
#include "../misc/util.t"
#include "../misc/flags.t"
#include "../rename/renameutil.t"
#include "../rename/deriv.t"
#include "../rename/rename.t" /* expandrecord */
#include "lettrans.t"
#include "hexpr.t"
#include "exprs.t"
#include "cutil.t"

export gender;
rec gender i t cs u = assocdefeq eqid i gtab (\_.\_.\u.(mkbnull, u)) t cs u

and gtab = [(hiEq, genEq); (hiOrd, genOrd); (hiIx, genIx); (hiEnum, genEnum)] @ if H1_3 then [(hiBounded, genBounded); (hiShow, genShow); (hiRead, genRead); (hiEval, genEval) /*; (hiFunctor, genFunctor)*/] else [(hiBinary, genBinary); (hiText, genText)]

and isenum cs = all (\(mkcons _ _ l _).null l) cs
and mkapi f a = mkap (mkident f) a
and mkap2i f x y = mkap (mkap (mkident f) x) y
and mkap3i f x y z = mkap (mkap (mkap (mkident f) x) y) z
and mkap4i f x y z w = mkap (mkap (mkap (mkap (mkident f) x) y) z) w
and etrue = mkconstr hctrue []
and efalse = mkconstr hcfalse []
and edum = mkident dummyid
and enever = mkident hinever
and etoEnumfail = mkapi hifail (estring "toEnum out of range")
and eq2 x y = mkap2i hieq x y
and andthem [x] [y] = eq2 x y
||  andthem (x.xs) (y.ys) = mkap2i hiand (eq2 x y) (andthem xs ys)
and orandthem [x] [y] = mkap2i hile x y
||  orandthem (x.xs) (y.ys) = mkap2i hior (mkap2i hilt x y) (mkap2i hiand (eq2 x y) (orandthem xs ys))
and orandthem3 [x] [y] _ = mkap2i hicompare x y
||  orandthem3 (x.xs) (y.ys) (z.zs) = 
	mkcase (mkap2i hicompare x y) [(mkconstr hcEQ [], orandthem3 xs ys zs); (z, z)]
and mkb bs = andify (map (\p.mkbpat [p]) bs)
and e0 = mkconst (cint 0)
and e1 = mkconst (cint 1)
and e10 = mkconst (cint 10)
and estr s = mkconst (cstring (tl s))
and estring s = mkconst (cstring s)
and eint i = mkconst (cint i)
and esp = mkapi hishowChar (mkconst (cchar ' '))
and elp = mkapi hishowChar (mkconst (cchar '('))
and erp = mkapi hishowChar (mkconst (cchar ')'))
and epair a b = mkconstr hcpair [a;b]
and ecomp f g = mkap2i hicomp f g
and comp [x] = x
||  comp (x.xs) = ecomp x (mkap2i hicomp esp (comp xs))
and comq [x] = x
||  comq (x.xs) = ecomp x (comq xs)
and comb i [e] = e
||  comb i (e.es) = mkap2i i e (comb i es)

and no_tvars (mktcons _ []) = true
||  no_tvars _ = false
and isderivd c (mkcons _ (_,[],[]) tbs _) = all (istder c o fst3) tbs
||  isderivd _ _ = false
and istder c (t as mktcons ti ts) = let r = istderi c ti & all (istder c) ts in /*trace ("istder "@prttype t@" "@show_bool r)*/ r
and istderi c (mkid _ _ (idi_type _ _ _ _ cis _) _) = assocdefeq eqid c (map getder cis) false
||  istderi _ _ = false -- I don't really see how this can happen. !!!
and getder (mkid _ _ (idi_inst (mkidecl _ i _ _) _ b) _, _) = (i, b)

and mko2 op x y = mkap2i op (mkapi hiord x) (mkapi hiord y)
and mkl2 op x y = mklam x (mklam y (mko2 op x y))
and mkl2s op x y = mklam y (mklam x (mko2 op x y))
and mkl2n op x y = mklam x (mklam y (mkap (mkident hinot) (mko2 op x y)))

and mkla2 op x y = mklam x (mklam y (mkap2i op x y))

and mkif c t e = mkcase c [(mkconstr hctrue [], t); (mkconstr hcfalse [], e)]

-- Generate equality test
and genEq t cs u =
    let [x1;y1] = for u (u+1) newid
    and u = u+2 in
    let ne = (mkident hine, mklam x1 (mklam y1 (mkapi hinot (mkap2i hieq x1 y1)))) in
    let [x;y] = for u (u+1) newid in
	if isenum cs then
	    (mkb    [(mkident hieq, mkl2 hieqint x y); (mkident hine, mkl2n hieqint x1 y1)], u+2)
	else
	  if no_tvars t & all (isderivd hiEq) cs & GenCmp then
	    let [x;y;x';y'] = for u (u+3) newid in
	    (mkb [(mkident hieq, mkla2 higeneq x y);
		  (mkident hine, mkla2 higenne x' y')], u+4)
	  else
	    let (pes, u') = Umap (geneqt y) cs (u+2) in
	    (mkb    [(mkident hieq, mklam x (mklam y (
			   if length cs = 1 then
			       mkcase x pes
			   else
			       mkcase (mkap2i hieqint (mkapi hicno x) (mkapi hicno y))
				      [(etrue, mkcase x pes);
				       (edum, efalse)]))
	            ); ne], u')

and geneqt y (mkcons i _ tbs _) u =
        let n = length tbs in
	let xs = for u (u+n-1) newid
        and ys = for (u+n) (u+2*n-1) newid
        and c = idtoconstr i in
        ((mkconstr c xs, mkcase y [(mkconstr c ys, if n = 0 then etrue else andthem xs ys); (edum, enever)]), u+2*n)

and eLT = mkident hiLT
and eGT = mkident hiGT
and eEQ = mkident hiEQ

-- Generate ordering tests
and genOrd t cs u =
	let [x;y;x1;y1] = for u (u+3) newid in
        let u = u+4 in
	let gt = (mkident higt, mklam x (mklam y (mkap2i hilt y x)))
	and ge = (mkident hige, mklam x1 (mklam y1 (mkap2i hile y1 x1))) in
	let! [x;y;x1;y1;x2;y2;x3;y3;x4;y4] = for u (u+9) newid in
        let u = u+10 in
	if isenum cs then
	    (mkb  ((if Ordering then
		    [(mkident hicompare, mklam x4 (mklam y4 (mkif (mko2 hiltint x4 y4) eLT (mkif (mko2 hiltint y4 x4) eGT eEQ))))]
		    else []) @
		    [(mkident hile, mkl2 hileint x1 y1);
		     (mkident hilt, mkl2 hiltint x2 y2);
		     (mkident hige, mkl2s hileint y3 x3);
		     (mkident higt, mkl2s hiltint y  x)]), u)
	else
	  if no_tvars t & all (isderivd hiEq) cs & all (isderivd hiOrd) cs & GenCmp then
	    -- XXX do something sensible with compare
	    (mkb [(mkident hilt, mkla2 higenlt  x  y);
		  (mkident hile, mkla2 higenle x1 y1);
		  (mkident hige, mkla2 higenge x2 y2);
		  (mkident higt, mkla2 higengt x3 y3)], u)
	  else
	   if Ordering then
	    let (pes, u') = Umap (genlet3 y) cs u in
            let xn = mkapi hicno x
            and yn = mkapi hicno y in
	      (mkb [(mkident hicompare, mklam x (mklam y (
			if length cs = 1 then
			    mkcase x pes
			else
			    (mkcase (mkap2i hieqint xn yn)
			            [(etrue, mkcase x pes);
				     (edum, mkif (mkap2i hiltint xn yn) eLT eGT)]))))
		   ], u')
	   else
	    let (pes, u') = Umap (genlet y) cs u in
            let xn = mkapi hicno x
            and yn = mkapi hicno y in
	        (mkb [(mkident hile, mklam x (mklam y (
			if length cs = 1 then
			    mkcase x pes
			else
			    (mkcase (mkap2i hieqint xn yn)
			            [(etrue, mkcase x pes);
				     (edum, mkap2i hiltint xn yn)])))
	         ); gt; ge], u')

and genlet y (mkcons i _ tbs _) u =
        let n = length tbs in
	let xs = for u (u+n-1) newid
        and ys = for (u+n) (u+2*n-1) newid
        and c = idtoconstr i in
        ((mkconstr c xs, mkcase y [(mkconstr c ys, if n = 0 then etrue else orandthem xs ys); (edum, enever)]), u+2*n)

and genlet3 y (mkcons i _ tbs _) u =
        let n = length tbs in
	let xs = for u (u+n-1) newid
        and ys = for (u+n) (u+2*n-1) newid
        and zs = for (u+2*n) (u+3*n-1) newid
        and c = idtoconstr i in
        ((mkconstr c xs, mkcase y [(mkconstr c ys, if n = 0 then eEQ else orandthem3 xs ys zs); (edum, enever)]), u+3*n)

and genIx t cs u =
	if isenum cs then
	    (mkb    [(mkident hirange, mkident hiPrange);
		     (mkident hiindex, mkident hiPindex);
		     (mkident hiinRange,mkident hiPinRange)], u)
	else
	    let [mkcons i _ tbs _] = cs in
            let ci = idtoconstr i in
	    let n = length tbs in

	    let newident = newidenti hirange in
	    let ls = for u (u+n-1) newident
            and us = for (u+n) (u+2*n-1) newident
            and is = for (u+2*n) (u+3*n-1) newident
            and x = newident (u+3*n) in
	    let rangecode = mklistg (mkconstr ci is) (map3 (\i.\l.\u.mkqgen i (mkapi hirange (epair l u))) is ls us) in
	    let drange = mklam x (mkcase x [(epair (mkconstr ci ls) (mkconstr ci us), rangecode)]) in

            let newident = newidenti hiindex in
	    let u = u+3*n+1 in
	    let ls = for u (u+n-1) newident
            and us = for (u+n) (u+2*n-1) newident
            and is = for (u+2*n) (u+3*n-1) newident
            and x = newident (u+3*n)
            and y = newident (u+3*n+1) in
            let sizes = map2 (\l.\h.mkap2i hiadd (mkap2i hiindex (epair l h) h) e1) ls us
            and indexes = map3 (\l.\h.\i.mkap2i hiindex (epair l h) i) ls us is in
            let indexcode =
		if length is = 1 then
		    hd indexes
		else
		    mkap2i hiadd
		           (reduce (\(i,s).\r.mkap2i himul (mkap2i hiadd r i) s) 
			           (mkap2i himul (hd indexes) (hd (tl sizes)))
			           (reverse (combine(tl indexes, tl(tl sizes)))))
			   (last indexes)
	    in
	    let dindex = mklam x (mklam y (mkcase x [(epair (mkconstr ci ls) (mkconstr ci us), mkcase y [(mkconstr ci is, indexcode)])])) in

            let newident = newidenti hiinRange in
	    let u = u+3*n+2 in
	    let ls = for u (u+n-1) newident
            and us = for (u+n) (u+2*n-1) newident
            and is = for (u+2*n) (u+3*n-1) newident
            and x = newident (u+3*n)
            and y = newident (u+3*n+1) in
            let inRangecode = comb hiand (map3 (\i.\l.\u.mkap2i hiinRange (epair l u) i) is ls us) in
	    let dinRange = mklam x (mklam y (mkcase x [(epair (mkconstr ci ls) (mkconstr ci us), mkcase y [(mkconstr ci is, inRangecode)])])) in

	    (mkb    [(mkident hirange, drange);
		     (mkident hiindex, dindex);
		     (mkident hiinRange, dinRange)], u+3*n+2)

and mkl3i i x y z = mklam x (mklam y (mklam z (mkap (mkap (mkap (mkident i) x) y) z)))
and mkl2i i x y = mklam x (mklam y (mkap (mkap (mkident i) x) y))
and genEnum t cs u =
   let (mkcons (hi as mkid _ _ (idi_constr _ _ _ hino _ _ _) _) _ _ _) = last cs
   and (mkcons lo _ _ _) = hd cs in
   let! [x1;x2;y2;x3;y3;z3;x4;y4;z4;x5;x6] = for u (u+10) newid in
   (mkb  ((if H1_3 then
	   [(mkident hitoEnum,		(mklam x5 (mkcase (mkap2i hior (mkap2i hiltint x5 e0) 
							               (mkap2i hiltint (mkconst (cint hino)) x5))
						          [(etrue, etoEnumfail);
							   (efalse, mkapi hichr x5)])));
	    (mkident hifromEnum,	(mklam x6 (mkapi hiord x6)))
	   ]
	  else []) @
           [(mkident hienumFrom,        (mklam x1 (mkap2i hienumFT x1 (mkident hi))));
	    (mkident hienumFromThen,    (mklam x2 (mklam y2 (mkap4i hienumFTTU x2 y2 (mkident lo) (mkident hi)))));
	    (mkident hienumFromTo,      mkl2i hienumFT  x3 y3);
	    (mkident hienumFromThenTo,  mkl3i hienumFTT x4 y4 z4)]), u+11)

and genText t cs u =
    let [x1;x2;x3;d1;d2] = for u (u+4) newid in
    let (pes1, u') = Umap (genshow d1) cs (u+5) in
    let (e3, u'') = genshowtype t x3 u' in
    let (e2, u''') = genread d2 x2 cs u'' in
    if GenRead then
	(mkb [(mkident hishowsPrec, mklam d1 (mklam x1 (mkcase x1 pes1)));
	      (mkident hireadsPrec, mklam d2 (mklam x2 e2));
	      (mkident hishowsType, mklam x3 e3) ], u''')
    else
	(mkb [(mkident hishowsPrec, mklam d1 (mklam x1 (mkcase x1 pes1)));
	      (mkident hishowsType, mklam x3 e3) ], u'')

and genShow t cs u =
    let [x1;x2;x3;d1;d2] = for u (u+4) newid in
    let (pes1, u') = Umap (genshow d1) cs (u+5) in
    let bshow = (mkident hishowsPrec, mklam d1 (mklam x1 (mkcase x1 pes1))) in
    let (e3, u'') = genshowtype t x3 u' in
    let bs = if ShowsType then [bshow; (mkident hishowsType, mklam x3 e3) ]
             else [bshow]
    in  (mkb bs, u'')

and genRead t cs u =
    let [x1;x2;x3;d1;d2] = for u (u+4) newid in
    let (e2, u') = genread d2 x2 cs (u+5) in
	(mkb [(mkident hireadsPrec, mklam d2 (mklam x2 e2)) ], u')

and isiso c = chr 160 <= c & c <= chr 255
and letter (_.c._) = isalpha c | isiso c | c = '_'

and genshow d (mkcons (i as mkid _ s _ (Orignames _ f (_,ons))) _ tbs flg) u =
    let n = length tbs in
    let xs = for u (u+n-1) newid in
    ((mkconstr (idtoconstr i) xs,
     if f = Nofixity & letter s then
	 if n = 0 then
	     mkapi hishowString (estr ons)
	 else if flg then
	     mkap2i hishowParen 
		    (mkap2i hileint e10 d) 
		    (comq (mkapi hishowString (estring (tl ons @ " {")).
			   concmap2 (\ (x,no) . \ (_,_,Some s) . 
				     [mkapi hishowString (estring (" "@oprid s@" = ")); 
				      mkap2i hishowsPrec e10 x] @
				     if no ~= n then
					 [mkapi hishowString (estring",")]
				     else
					 []
				    ) (combine (xs,[1..])) tbs @
			   [mkapi hishowString (estring " }")]))
	 else
	     mkap2i hishowParen (mkap2i hileint e10 d) (comp (mkapi hishowString (estr ons).map (mkap2i hishowsPrec e10) xs))
     else
	 let (lp, rp, p) = case f in InfixL p : (p, p+1, p) || InfixR p : (p+1, p, p) || Infix p : (p+1,p+1, p) || Nofixity : (9, 10, 9) end in
         let [e1;e2] = xs in
         mkap2i hishowParen (mkap2i hiltint (eint p) d) (comp [mkap2i hishowsPrec (eint lp) e1; mkapi hishowString (estr ons); mkap2i hishowsPrec (eint rp) e2])),
     u+n)

and genshowtype kty x u =
    let (ty as mktcons ti tvs) = tpart kty
    and k = cpart kty in
    let n = length tvs in
    if n = 0 then
	(mkapi hishowString (estr (snd (id_orignames ti))), u)
    else
	let mkf v = [esp; mkapi hishowsType (mkap (mkinfo (restr [] (xmkcontype k (mktcons hiARROW [ty; v]))) (mkident hishowt)) x)] in
	(comb hicomp ([elp; mkapi hishowString (estr (snd (id_orignames ti)))] @ concmap mkf tvs @ [erp]), u)

and genread d x cs u =
    let (ecs, u') = Umap (genreadc d x) cs u in
    (comb hiconc ecs, u')

and genreadc d x (mkcons (i as mkid _ s _ (Orignames _ f (_,ons))) _ tbs flg) u =
    let n = length tbs in
    let r1 = newid u in
    let ss = for (u+1) (u+n+1) newid
    and tt = for (u+n+2) (u+2*(n+1)) newid in
    let c = idtoconstr i in
    let cname = estr ons in
    ((if f = Nofixity & letter s then
       if flg then
--	fail "Cannot derive read for records yet."
        let rr = [ epair (estring (oprid l)) 
                         (ecomp (mkapi himapFst (mklam v1 (mklam v2 (expandrecord false v2 [l] [v1]))))
                                (mkident hireads)) 
                   ;;
                   (v1, (v2, (t,b,Some l))) <- combine (ss,combine(tt,tbs)) ] in
        mkap3i hireadParen efalse
         (mkap3i hireadRec (elist rr) (expandrecord false (mkident i) [] []) cname)
         x
       else
	mkap3i hireadParen (if n = 0 then efalse else (mkap2i hileint e10 d))
	       (mklam r1 (mklistg (epair (mkconstr c (tl tt)) (last ss))
				  (mkqgen (epair (hd tt) (hd ss)) (mkapi hilex r1) . mkqfilter (mkap2i hieq (hd tt) cname) .
				   map3 (\sk.\tk.\sk1.mkqgen (epair tk sk) (mkap2i hireadsPrec e10 sk1)) (tl ss) (tl tt) ss)
				  ))
               x
      else
	let (lp, rp, p) = case f in InfixL p : (p, p+1, p) || InfixR p : (p+1, p, p) || Infix p : (p+1,p+1, p)  || Nofixity : (9, 10, 9) end in
        let [s0; s1; s2] = ss
        and [u; tok; v] = tt in
	mkap3i hireadParen (mkap2i hiltint (eint p) d)
	       (mklam r1 (mklistg (epair (mkconstr c [u;v]) s2)
			          [mkqgen (epair u s0) (mkap2i hireadsPrec (eint lp) r1);
				   mkqgen (epair tok s1) (mkapi hilex s0);
				      mkqfilter (mkap2i hieq tok cname);
				   mkqgen (epair v s2) (mkap2i hireadsPrec (eint rp) s1)]))
	       x
      ), u+2*(n+1)+1)

and genBinary t cs u =
    (mkb    [(mkident hishowBin, mkident hiPshowBin);
	     (mkident hireadBin, mkident hiPreadBin)], u)

and genBounded t cs u =
    let imin = mkident himinBound
    and imax = mkident himaxBound in
    if isenum cs then
	let (mkcons hi _ _ _) = last cs
	and (mkcons lo _ _ _) = hd cs in
	    (mkb    [(imin, mkident lo);
		     (imax, mkident hi)], u)
    else
	let [mkcons i _ tbs _] = cs in
	let ci = idtoconstr i in
	let n = length tbs in
	    (mkb    [(imin, mkconstr ci (rept n imin));
		     (imax, mkconstr ci (rept n imax))], u)

and genEval t cs u = (mkbnull, u)

and genFunctor t cs u = fail "deriving Functor not implemented yet"
end
