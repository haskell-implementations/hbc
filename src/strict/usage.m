module
#include "../expr/id_t.t"
#include "../expr/constr_t.t"
#include "../expr/impexp_t.t"
#include "../ExprE/Expr_t.t"
#include "../ExprE/Eprint.t"
#include "../expr/einfo_t.t"
#include "../expr/einfo.t"
#include "../expr/booltree.t"
#include "../expr/id.t"
#include "../misc/flags.t"
#include "../misc/util.t"
#include "../misc/misc.t"
#include "../misc/sort.t"
#include "strict.t"
export usageanal, compr;
rec usageanal :: Expr -> Expr -> [(Id # (Int # BT # BT))] -> Expr
and usageanal oe ne sinf = 
    case useinfo ne sinf in
	[] : ne
    ||  einf : 
            let sinf' = map (addstrict einf) sinf in
	    let einf' = filter (Or o snd) einf in
	    let nne = dostrictanal oe sinf' einf' in
--	    usageanal oe nne sinf'		-- better, but slower
(if X4 then trace ("old sinf="@show_sinf sinf@"\nnew sinf="@show_sinf sinf'@"\n\nimproved: "@xxximpr sinf sinf') else (\x.x))
            nne
    end

and xxximpr l1 l2 = show_sinf (conc (map2 xxximpr1 l1 l2))
and xxximpr1 (i,(_,old,_)) (nn as (_,(_,new,_))) = if old=new then [] else [nn]

and type SINF == [(Id # (Int # BT # BT))]
and show_sinf = show_list (\(i, (_,s,_)).prid i@":"@show_BT s)

and addstrict einf (p as (i, (a, s, t))) =
    case assocdefeq eqid i einf [] in
	[] : p
    ||  bs : (i, (a, btsimpl (btands [s; btands (map2 (\b.\n.if b then btvar n else bttt) bs (from 0))]), t))
    end

and type IFS == [(Int # Finfo)]

and arity sinf f = case assocdefeq eqid f sinf (-1,fail"",fail"") in (a,_,_):a end

-- Collect information local usage of all functions and try to 
-- improve strictness with this.
and useinfo :: Expr -> SINF -> [Id # [Bool]]
and useinfo (Emodule _ exps dss) sinf =
    let ifs = smkinfotab sinf in
    let chkids = filter (not o id_is_visible) (map fst sinf) in
    let adss = map (mapsnd (abstr ifs chkids)) dss in
    let str = processblks sinf chkids adss [] in
(if X4 then
    trace ("cis="@show_list prid chkids@"\n"@
           "absfun= "@show_list (show_pair(prid,show_Appls)) (conc adss)@"\n"@
	   "str="@show_Appls str
	  )
else (\x.x))
    (mapsnd (map (=bttt)) str)

and compr (b.bs) (x.xs) = if b then x.compr bs xs else compr bs xs
||  compr _ _ = []

-- Make an abstraction of a function regarding evaluation status.
-- The result is a list of (abstract) function applications that can occur,
-- parametrized over the arguments. tt is evaluted, ff is unevaluated
and c_unev = btff
and c_ev = bttt
and type Appl == (Id # [BT])
and type Appls == [Appl]
and abstr :: IFS -> [Id] -> Expr -> Appls
and abstr ifs cis (Elaml is e) = abse ifs cis (combine(is, map btvar (from 0))) e
||  abstr ifs cis e = abse ifs cis [] e
and abse ifs cis env e = (S env e where
rec S r (Econstr _ es) = concmap (S r) es
||  S r (Elet _ ies e) = concmap (S r o snd) ies @ S (map (L r) ies @ r) e
||  S r (Ecase e cies ed) = S r e @ concmap (Sc r) cies @ S r ed
||  S r (Efailmatch _) = []
||  S r (Ecfunction _ _) = []
||  S r (e as Eidapl i es) & (member eqid i cis) = 
    case sidinfo ifs i in
	finfo a _ _ _ _ & (a >= 0 & length es = a) : /*trace ("added "@prid i@" "@show_list show_BT (map (E r) es))*/ ((i, map (E r) es).concmap (S r) es)
    ||  finfo a _ _ _ _ : /*trace ("arity of "@prid i@" is "@itos a@" for "@pr e)*/ ((i, rept a c_unev).concmap (S r) es)
    end
||  S r (e as Eidapl _ es) = /*trace ("discard "@pr e)*/ (concmap (S r) es)
||  S r (Einfo _ e) = S r e
||  S r e = fail ("No match in S (usage analysis): "@pr e)
and Sc r (c,is,e) = S r e -- ignore strict constructors for now
and L r (i,e) = (i, E r e)
and E r (Ecase _ cies ed) = btsimpl (btands (E r ed.map (\(_,_,e).E r e) cies))
||  E r (Elet _ ies e) = E (map (L r) ies @ r) e
||  E r (Eidapl i []) = assocdefeq eqid i r c_unev
||  E r (Eidapl i es) =
    case sidinfo ifs i in
	finfo (-1) _ _ _ _ : c_unev	-- unknown function called
    ||  finfo a _ (_,trm) _ _ : 
		if length es ~= a then 
		    c_unev		-- could be bttt if l<a, but be careful with functional args
		else
		    let bs = map (E r) es in	-- collect status for all arguments
		    let b = btsimpl (substbt bs trm) in
--    trace ("fun "@prid i@" term="@show_BT trm@" has args "@show_list show_BT bs@" is "@show_BT b)
		    b
		    
    end
||  E r (Econstr _ _) = c_ev
||  E r (Ecfunction _ _) = c_ev
||  E r (Einfo noeval _) = c_ev
||  E r (Einfo strict _) = c_ev
||  E r (Einfo _ e) = E r e
||  E r (Efailmatch n) = if n = 0 then c_ev else c_unev
||  E _ e = fail ("No match in E (usage analysis): "@pr e)
)

and show_Appls = show_list (show_pair(prid,show_list show_BT))

and processblks :: SINF -> [Id] -> [[(Id # Appls)]] -> Sol -> Sol
and processblks sinf cis [] r = r
||  processblks sinf cis (blk.blks) r = 
	let t = processblk sinf cis blk blks in
	processblks sinf cis blks (r@t)
and processblk :: SINF -> [Id] -> [(Id # Appls)] -> [[(Id # Appls)]] -> Sol
and processblk sinf cis blk blks =
	let fs = map fst blk in
	let aps = concmap (concmap snd) blks in
	let g f aps =
	    if member eqid f cis then
		let ips = filter (\(i,b).eqid f i) aps in
		simps (arity sinf f) ips
	    else
		rept (arity sinf f) c_unev
        in
	let faps = map (\f.(f, g f aps)) fs in
	let flaps = mapsnd (filter (\(i,_).member eqid i fs)) blk in
	let nfaps = solve sinf flaps faps in
--	trace ("processblk faps="@show_list (show_pair (prid, show_list show_BT)) faps@"\n"
--	       @"flaps="@show_list (show_pair(prid,show_Appls)) flaps
--              )
        nfaps
and chktt (b as btands []) = b
||  chktt _ = btff

and simps :: Int -> [(Id#[BT])] -> [BT]
and simps n [] = rept n c_ev
||  simps _ ips = map (chktt o btsimpl o btands) (transpose (map snd ips))

and type Sol == [(Id # [BT])]

and solve :: SINF -> [(Id # Appls)] -> Sol -> Sol
and solve sinf eqns sol =
	let res = concmap (\(i,xs). mapsnd (substeq (assocdefeq eqid i sol (fail "assoc-solve"))) xs) eqns in
	let sol' = map (\(eqn as (f,_)). (f, simps (arity sinf f) (eqn.filter (\(i,_).eqid f i) res)) ) sol in
--trace ("solve res="@show_Appls res@"\nsol ="@show_Appls sol@"\nsol'="@show_Appls sol')
(
	if eqsol sol' sol then sol else solve sinf eqns sol'
)
and eqsol xs ys = And (map2 (\(_,x).\(_,y).x=y) xs ys)
and substeq :: [BT] -> [BT] -> [BT]
and substeq bs bts = map (substbt' bs) bts
and substbt' s b = substbt s (btsimpl b)
end

