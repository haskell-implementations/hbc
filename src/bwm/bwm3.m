module
#ifdef BWM
#include "../expr/id_t.t"
#include "../expr/id.t"
#include "../expr/ttype.t"
#include "../expr/constr_t.t"
#include "../misc/util.t"
#include "../misc/misc.t"
#include "../expr/constrfun.t"
#include "../transform/hexpr.t"
#include "../funnos.h"
#include "bwm2.t"
export bwmstk, maxnode, maxlit, hpminoffs, hpmaxoffs;
rec bwmstk dss = map (map stkf) dss
and stkf (i, is, e) = (i, is, toppull (isenv is) e)
and toppull r0 e =
    let rec (u', (bs, e')) = pull 10000 e
    and     (u'', e'') = precase u' (blet bs e')
    and     r   = r0@buildmap 0 e''
    in varadj r e''
and blet [] e = e
||  blet bs e = Blet bs e
and look m i = assocdefeq eqbv i m i
and addstk n [] = []
||  addstk n ((i, Bvstack k i').xs) = (i, Bvstack (k+n) i').addstk n xs
||  addstk n (x.xs) = x.addstk n xs
and isenv is = map2 (\i.\n.(i,Bvstack n (bvar2id i))) is (from 0)

and stke r e = e

and buildmap _ (Bcase _ _ cies) = concmap (\ (_,_,e).buildmap 0 e) cies
||  buildmap n (Blet ies e) = 
    let (n', m) = mapstate onemap n ies in
    m @ buildmap n' e
||  buildmap n e = []
and onemap n (Bvalures, _) = (n, (Bvalures, Bvalures))
||  onemap n (i, _) = (n+1, (i, Bvheap n (bvar2id i)))

and newvar p u = Bvheap (-1) (mkid u (p@itos u) idi_udef Noorigname)
and snewvar p u = Bvstack (-1) (mkid u (p@itos u) idi_udef Noorigname)

and varadj r (Blet ies e) = Blet (map (varadjie r) ies) (varadj r e)
||  varadj r (Bcase e ts cies) = Bcase (varadj r e) (map (look r) ts) (map (varadjcie (isenv ts@addstk (length ts) r)) cies)
||  varadj r (Bapply v es) = Bapply (look r v) (map (varadj r) es)
||  varadj r (Bvar v) = Bvar (look r v)
and varadjie r (i, e) = (look r i, varadj r e)
and varadjcie r (c,is,e) =
    (c, is, varadj (isenv is@addstk (length is+if eqtype (ctype c) Tint then 0 else 1) r) e)

and pull u (Blet ies e) =
    let (u', bss) = mapstate pullie u ies in
    let (u'', (bs, e')) = pull u' e in
    (u'', (conc bss@bs, e'))
||  pull u (Bcase e ts cies) = 
    let (u', (bs, e')) = pull u e in
    let (u'', e'') = maxapply (nodesize1-1) u' e' in
    let (u''', cies') = mapstate pullcie u'' cies in
    let (u'''', e''', b) = nocmplx u''' e'' in
    (u'''', (bs@b, Bcase e''' ts cies'))
||  pull u (Bapply v es) =
    let (u', bses) = mapstate pullb u es in
    let (bss, es') = split bses in
	(u', (conc bss, Bapply v es'))
||  pull u (Bvar v) = (u, ([], Bvar v))

and pullb u (Bvar v) = (u, ([], Bvar v))
||  pullb ou oe =
    let (u, e) = maxapply nodesize1 ou oe in
    let i = newvar "h" u in
    let (u', (bs, e')) = pull (u+1) e in
    (u', (bs@[(i, e')], Bvar i))

and pullie u (i, e) =
    let (u', (bs, e')) = pull u e in
    (u', (bs@[(i, e')]))
and pullcie u (c,is,e) =
    let (u', (bs, e')) = pull u e in
    (u', (c, is, blet bs e'))

and nocmplx u (e as Bvar _) = (u, e, [])
||  nocmplx u (e as Bapply _ _) = (u, e, [])
||  nocmplx u e = let c = newvar "c" u in (u+1, Bvar c, [(c, e)])

and precase u (Blet bs e) = prelet u [] bs e
||  precase u (Bcase e ts cies) =
    let (u', cies') = mapstate precie u cies in
    (u', Bcase e ts cies')
||  precase u e = (u, e)
and precie u (c, is, e) =
    let (u', e') = precase u e in
    (u', (c, is, e'))
and prelet u bs [] e = bletu bs (precase u e)
||  prelet u bs ((v,Bapply i [b]).ves) e & (id_no (bvar2id i) = Ford | id_no (bvar2id i) = Fchr) = prelet u (bs@[(v,b)]) ves e
||  prelet u bs ((v,Bapply i es).ves) e & (id_is_predef (bvar2id i)) = bletu bs (bbldcase u v i es [] ves e)
||  prelet u bs (b.ves) e = prelet u (bs@[b]) ves e
and bbldcase u v i []     is ves e = bindbox u v (Bapply i is) ves e
||  bbldcase u v i (b.es) is ves e = 
    let ii = snewvar "i" u in
    let (u', e') = bbldcase (u+1) v i es (is@[Bvar ii]) ves e in
    (u', bcase b [/*!!!*/] [(boxcon, [ii], e')])
and boxcon = 
    let tt = Tint in
    Cconstr "BOX" tt (mktinfo tt 1 false true [] false false None) 0 (false,[],[]) []
and Bbox e = Bapply (Bvconstr 0 1 boxcon) [e]
and bletu bs (u, e as Bapply i _) & (id_is_predef (bvar2id i)) = 
    let ii = snewvar "h" u in
    prelet (u+1) bs [(ii, e)] (Bvar ii)
||  bletu bs (u, e) = (u, blet bs e)
and bcase (c as Bvar (Bvconstr _ _ _)) ts [(_,[i],e)] = Bsubst c i e
||  bcase b ts bs = Bcase b ts bs
and bindbox u v bv ves e & (heapvar v) = 
    case ves in
	((v',ve').ves') & (contains v ve') : prelet u [(aluvar, Bbox bv)] ((v', Bsubst (Bvar aluvar) v ve').ves') e
    ||  [] & (contains v e) : prelet u [(aluvar, Bbox bv)] [] (Bsubst (Bvar aluvar) v e)
    ||  _ : prelet u [(v,Bbox bv)] ves e
    end
||  bindbox u v bv ves e = prelet u [(v,Bbox bv)] ves e
and heapvar v = hd (idtostr (bvar2id v)) = 'h'
and aluvar = Bvalures

and contains v (Bvar v') = eqbv v v'
||  contains v (Bapply _ es) = exists (contains v) es
||  contains v (Bcase e _ _) = contains v e
||  contains v _ = false


and Bsubst t v (Bvar v') & (eqbv v v') = t
||  Bsubst t v (e as Bvar _) = e
||  Bsubst t v (Blet bs e) = Blet (mapsnd (Bsubst t v) bs) (Bsubst t v e)
||  Bsubst t v (Bcase e ts cies) = Bcase (Bsubst t v e) ts (map (\ (c,is,e). (c, is, Bsubst t v e)) cies)
||  Bsubst t v (Bapply v' es) = Bapply v' (map (Bsubst t v) es)

and nodesize = maxnode
and nodesize1 = nodesize-1
-- make sure max arg count is not exceeded
and maxapply k u (e as Bapply v es) =
    if length es <= k then
	(u, e)
    else
        let i = newvar "v" u in
	let (u', e') = maxapply nodesize1 u (Bapply i (tail k es)) in
        (u', Blet [(i, Bapply v (head k es))] e')
||  maxapply _ u e = (u, e)


and findarg [] _ d = d
||  findarg (('-'.a)._) s _ & (s = head (length s) a) = stoi (tail (length s) a)
||  findarg (_.xs) s d = findarg xs s d

and defnode = 4
and maxnode = findarg argv "maxnode" defnode
and maxlit = findarg argv "maxlit" (maxnode-2)
and hpminoffs = findarg argv "hpminoffs" 0
and hpmaxoffs = findarg argv "hpmaxoffs" 16

#else
export ;
dummy=0
#endif
end

-- lift case from non-return positions
-- handle let in Bcase





