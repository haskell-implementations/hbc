module
#include "../expr/types_t.t"

export substear;

-- Substitute lml expressions for bracketed concrete expressions
rec substear expl e = snd (isubstear expl e)
and isubstear expl (mkap e1 e2) = 
      let (expl',e1') = isubstear expl e1 in
      let (expl'',e2') = isubstear expl' e2 
      in (expl'', mkap e1' e2')
|| isubstear expl (mklam e1 e2)=
      let (expl',e2') = isubstear expl e2
      in (expl', mklam e1 e2')
|| isubstear expl (mkcase e1 pl) =
      let (expl',e1') = isubstear expl e1 in
      let (expl'',pl') = 
        mapstate (\exps.\(pt,et).
                   let (exps',pt') = isubstear exps pt in
                   let (exps'',et') = isubstear exps' et 
                   in (exps'', (pt',et'))
                 ) 
                 expl'
                 pl
      in (expl'', mkcase e1' pl')
|| isubstear expl (mkletv b e1) =
      let (expl',b') = isubstearbi expl b in
      let (expl'',e1') = isubstear expl' e1
      in (expl'', mkletv b' e1')
|| isubstear expl e as mkident _ = (expl, e)
|| isubstear expl (mkmodule mid dk imp exp b) = 
      let (expl',b') = isubstearbi expl b
      in (expl', mkmodule mid dk imp exp b')
|| isubstear expl e as mkconst _ = (expl,e)
|| isubstear expl e as mkcfunction _ _ = (expl,e)
|| isubstear (h.expl') (mkbrack _ _) = (expl',h)
|| isubstear expl e as mkerror _ = (expl,e)
|| isubstear expl (mkas id e) = 
      let (expl',e') = isubstear expl e 
      in (expl', mkas id e')
|| isubstear expl (mkcondp e1 e2) =
      let (expl', e1') = isubstear expl e1 in
      let (expl'',e2') = isubstear expl' e2 
      in (expl'', mkcondp e1' e2')
|| isubstear expl (mkconstr c el) = 
      let (expl',el') = mapstate (\exps.\e. isubstear exps e) expl el
      in 
         (expl',mkconstr c el')
|| isubstear expl e as mkfailmatch n = (expl,e)
|| isubstear expl (mkinfo restr e) = 
      let (expl',e') = isubstear expl e
      in (expl', mkinfo restr e')
-- || isubstear expl (mklistf n e) = 
-- || isubstear expl (mklistg e Ql) = 
and
   isubstearbi expl (b as mkbtype _ _ _ _) = (expl,b)
|| isubstearbi expl (mkbctype _ _) = fail "Mkbctype in isubstearbi"
|| isubstearbi expl (mkbpat [(i,e)]) = 
      let (expl',e') = isubstear expl e
      in 
          (expl',mkbpat [(i,e')])
|| isubstearbi expl (mkbpat el) =
 -- listan i mkbpat har inte alltid ett element????????
      let (expl',el') = 
         reduce (\ (e1,e2).\ (expl,rel).
                 let (expl',e1') = isubstear expl e1 in
                 let (expl'',e2') = isubstear expl' e2 
                 in (expl'', (e1',e2').rel)
                ) 
                (expl,[])
                el
      in (expl', (mkbpat el'))
 
|| isubstearbi expl (mkbmulti e1 e2) = 
      let (expl', e1') = isubstear expl e1 in
      let (expl'',e2') = isubstear expl' e2 
      in (expl'', mkbmulti e1' e2')
|| isubstearbi expl (mkband b1 b2) =
      let (expl', b1') = isubstearbi expl b1 in
      let (expl'',b2') = isubstearbi expl' b2 
      in (expl'', mkband b1' b2')
|| isubstearbi expl (mkbrec b) =
      let (expl',b') = isubstearbi expl b
      in (expl',mkbrec b')
|| isubstearbi expl (b as mkberror _) = (expl,b)
|| isubstearbi expl (mkblocal b1 b2) =
      let (expl', b1') = isubstearbi expl b1 in
      let (expl'',b2') = isubstearbi expl' b2 
      in (expl'',mkblocal b1' b2')
|| isubstearbi expl mkbnull = (expl, mkbnull) -- compiler bugg ??
|| isubstearbi expl (b as mkbsyn _ _) = (expl,b)
|| isubstearbi expl (b as mkbinstance _ _ _) = (expl,b)
|| isubstearbi expl (b as mkbdefault _) = (expl,b)
|| isubstearbi expl (b as mkbsign _ _) = (expl,b)
|| isubstearbi expl (b as mkbpragma _) = (expl,b)

end
