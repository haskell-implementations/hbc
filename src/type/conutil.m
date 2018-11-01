module  -- conutil
#include "../expr/id_t.t"
#include "../expr/id.t"
#include "../expr/idtab.t"
#include "../expr/ttype_t.t"
#include "../expr/ttype.t"
#include "../expr/einfo_t.t"
#include "../ExprE/Expr_t.t"
#include "../ExprE/Eprint.t"
#include "../ExprE/Eutil.t"
#include "../misc/misc.t"
#include "../misc/util.t"
#include "../misc/flags.t"
#include "../misc/sort.t"
#include "../transform/hexpr.t"
#include "subst_t.t"
#include "subst.t"
#include "prefix.t"

export combcon, combdict, combdicts, issuper, caply, subsume, findit, matchs, matcht, findinst,
       newd, resolvedefs, buildid, flatcollaps, reorder, himplies, substcon, handleD, xrebuild,
       resolvtyvars;

rec caply e es = reduce (\a.\r.Eap r a) e (reverse es)

-- combine dictionaries
and combdict D1 D2 = D1 @ D2
and combdicts Ds = conc Ds

-- is s a superclass of c
and issuper s c = member eqid s (map fst (getsups c))
and getsups (c as mkid _ _ (idi_class (clsi _ _ _ sup _ _)) _) = sup
||  getsups c = fail ("No match in getsups "@prid c)

-- combcon combines two contexts keeping them minimized
and combcon [] k = k		-- fast special case
||  combcon k [] = k		-- fast special case
||  combcon k1 k2 = 
let rrr =
reduce add1con k2 (reverse k1)	-- the reverse avoids upsetting the order in k1
in
if TestN > 1 then trace (force ("combcon "@show_list prassert k1@" and "@show_list prassert k2@" is "@show_list prassert rrr)) rrr else
rrr

and prassert = prttype o as2ty

and issuperk (mkassert i1 v1) (mkassert i2 v2) = v1=v2 & issuper i1 i2
and add1con k [] = [k]
||  add1con (k1 as mkassert i1 v1) (kks as (k2 as (mkassert i2 v2)).ks) & (v1 = v2) =
    if eqid i1 i2 | issuper i1 i2 then
	kks
    else if issuper i2 i1 then
	k1.filter (\(mkassert ix vx).~(v1=vx & issuper i1 ix)) ks
    else
	k2.add1con k1 ks
||  add1con k1 (k.ks) = k.add1con k1 ks
	
-- minimise a flat assertion list
and subsume k = reduce add1con [] (reverse k)

-- Resolving overloading stuff

and dmknewid (mkid _ _ (idi_class (c as clsi _ _ _ _ _ _)) _) s n = mkid n (s@itos n) (idi_var (var_dict c) Onotype None) noorigname
and buildid (mkassert i _) n = dmknewid i ("dict"@idtostr i) n

-- Take an assertion list and turn it into a non-flat context via a substitution
and substcon al k = map (\(mkassert d [v]).(d, assocdef v al (mktvar v))) k -- ASSERT

and nearsup ci di =
	selsuper (getsups ci) di
-- find closest superclass
and selsuper xs di =
	snd (hd (sort (\(_,l1).\(_,l2).length l1 < length l2) (filter (\(s,_).eqid s di) xs)))

-- take a list of (Id, Assert) pairs and a non-flat context and turn this into the appropriate dictionary expression
and rebuild iks dv = xrebuild Emkint Evar scaply iks dv
and xrebuild xint xvar xap iks (di, mktvar tv) =
    let (i, (mkassert ci _)) = findk iks di tv in
    let is = if eqid ci di then [] else nearsup ci di in
    xap i (map xint is)
||  xrebuild xint xvar xap iks (di, ot) =
    case while is_syn synexpand ot in
        (t as mktcons tt ts) :
            let (mvec, mkidecl k _ [mktcons ti tvs] _) = findinst t di in -- ASSERT
	    let su = matchs tvs ts in
            let c = substcon su k in
            xap (xvar mvec) (map (xrebuild xint xvar xap iks) c)
    ||  (t as mktvar _) : xrebuild xint xvar xap iks (di, t)
    ||  t : fail ("rebuild"@prttype (mktcons di [t]))
    end

and scaply i es = caply i es  --  ASSERT
and findk iks ci tv = findf iks (\(i, (mkassert xi [v])). tv=v & (eqid xi ci | issuper ci xi)) (\x.x) (fail ("findk "@prid ci))

-- find the instance decl for tt in class c
and findinst t (c as mkid _ _ (idi_class (clsi _ _ _ _ insts _)) _) = 
    case findit c t insts in
        [x] : x
    ||  _ :  fail ("Bad findinst "@prttype t@" in "@prid c)
    end


and gtypeinsts i = 
	if Interactive | isskolemtype i then 
	    let (mkid _ _ (idi_type _ _ _ _ insts _) _) = i in insts
        else 
	    []         -- Types are more up to date in the interactive system

and findit ci (t as mktcons ti _) is = 
    case findit_t t is in
       [] : findit_t t (filter (\ (_,id).eqid (iclsname id) ci) (gtypeinsts ti))
    || xs : xs 
    end

-- Find the (method vector, instance decl) for the type xt in a list of instances for a class
and findit_t xt ps = 
(if TestN > 1 then trace ("findit_t "@prttype xt@" "@show_list (show_pair((\x."?"),pridecl)) ps) else (\x.x))
(
	case filter (\(_, t).match (itype t) xt ~= None) ps in
	    (is as _._._) : -- find the most specific instances
		let xs = filter (\(_,i). all (lti (itype i)) is) is in
		if AllowRedef then
		    mkseteq (\(_,mkidecl _ _ t1 _).\(_,mkidecl _ _ t2 _).And (map2 eqtype t1 t2)) xs
		else
		    xs
	||  is : is	-- fast case for [] and [x]
	end
)
and lti t1 (_,i2) = match (itype i2) t1 ~= None

and eqa (mkassert i1 v1) (mkassert i2 v2) = v1=v2 & eqid i1 i2

-- Does k imply y?
and himplies k y =
    case flatcon y in
	No s : false
    ||  Yes nk : all (\(mkassert d v).exists (\(mkassert c w).v=w & (eqid c d | issuper d c)) k) nk
    end

and prD1 x = show_pair(prid,show_pair(prid,prttype)) x
and prD d = show_list prD1 d

-- if some of the type vars in a complex expression are in the context are in trouble
and badhandleD k (_,(_, t)) =
    let kvars = concmap (\(mkassert _ v).v) k
    and tvars = getTvars t in
    intersect kvars tvars ~= []

-- Given a list of (Id, Assert) pairs, a context, and a (Id, (ClassId, Ttype)) pair, build the expression to select the Ttype-dictionary
and handleD iks k (j,y) & (himplies k y) = 
let rrr = Yes (j, rebuild iks y)
in
if Test then trace ("handleD D-entry=("@prid j@" ~> "@show_pair(prid,prttype) y@") to "@pr (rebuild iks y)) rrr else
rrr
--||  handleD iks k (j,y) & (badhandleD k y) = trace ("Bad handleD: j="@prid j@", k="@show_list prassert k@", y="@show_pair(prid,prttype) y) (No (j,y))
||  handleD _   _ p = No p

and itlookid it d = itlookupdef it d (fail ("newd: "@prid d))
/*
    case itlookupdef it d (fail ("newd: "@prid d)) in
       Evar i : i
    || e : fail ("itlookid "@pr e)
    end
*/

and istvar (mktvar _) = true
||  istvar _ = false
-- Given a substitution, a context, and dictionary vars transform a (id, expr) pair & a type & a dictionary mapping
-- to a new (id, expr) pair & a new type & a new dictionary mapping & ?
and newd it R curk0 is0 (i,oe) r D0 hasrestr u =
    let D = TRdict R D0 in
    let (is, curk) = if hasrestr then
		   let vs = getTvars r in split [ (i, mkassert c xs);; (i, mkassert c xs) <- combine (is0,curk0); all (\x.mem x vs) xs]
	       else
		   (is0, curk0)
	       in
    let rec  lk = length curk
    -- add lambdas for givens ids
    and      e = reduce xElam oe is
    and      iks = combine (map Evar is, curk)
    -- build a mapping of requested stuff if we know it by now
    and      (ies, Dns) = ynsplit (map (handleD iks curk) D) [] []
    and      niks = map (\(d, (cl, mktvar v)).(itlookid it d, mkassert cl [v])) (filter (\(_, (_, t)).istvar t) Dns) -- ASSERT
    and      (Dnsbad, Dnsok) = partition (badhandleD curk) Dns
    and      (ies', Dnsbad') = ynsplit (map (handleD (iks@niks) (map snd niks @ curk)) Dnsbad) [] []
    in
let rrr =
	if lk = 0 & ies = [] & ies' = [] then		-- fast special case
	    (((i, oe), D,   r,                 []),  u)
	else
	    (((i, e),  Dnsbad'@Dnsok, xmkcontype curk r, ies@ies'), u)
in
if Test then trace (force ("newd i="@prid i@"\n    R="@prTR R@"\n    r="@prttype r@"\n    is="@show_list prid is@"\n    curk="@show_list prassert curk@"\n    mapped "@show_list (show_pair(prid,butlast o pr)) ies)@"\n    Dns="@prD Dns@ (if null Dnsbad then "" else "\n    Dnsbad="@prD Dnsbad@"\n    Dnsbad'="@prD Dnsbad'@"\n    ies'="@show_list (show_pair(prid,butlast o pr)) ies')) rrr else
rrr

and xElam i (Einfo (f as forcearity _) e) = Einfo f (Elam i e)
||  xElam i e = Elam i e

and resolv1tyvar k defs (p as (v, cis)) =
let rrr =
    if (H1_3 | all (\ci.member eqid ci stdClass | member eqid ci numClass) cis) & exists (\ci.member eqid ci numClass) cis then
	case findf defs (\t.all (\ci.himplies k (ci, t)) cis) Some None in
	    None : No ("[54] Impossible to resolve ambiguously overloaded tyvar(s) in class(es) "@mixmap oprid cis ", ")
        ||  Some t : Yes (v, t)
        end
    else
	No ("[55] Ambiguously overloaded tyvar(s) in class(es) "@mixmap oprid cis ", ")
in
if TestN > 0 then
trace ("resolv "@itos v@" "@show_list prid cis@" "@show_bool (all (\ci.member eqid ci stdClass | member eqid ci numClass) cis & exists (\ci.member eqid ci numClass) cis)@" "@show_list (\t.show_list (\ci.show_bool (himplies k (ci, t))) cis) defs) rrr
else rrr


and aTR (ok _ k _) = k
||  aTR _ = []

and resolvedefs R D defs p tvs =
    let kk = [] in			-- use context from current subst?
    let kkk = aTR R in
    case oktestconc (map (\(i, y).flatcon y) D) in
	No s : (bad [s])		-- Can this ever happen?
    ||  Yes k :
	let xs = union (getngs p) tvs in
	let kg = filter (\(mkassert _ vs).all (\v. ~mem v xs) vs) k in
        let kgvs = mkset (getavars kg) in
        let kg1 = mkseteq eqa (filter (\(mkassert _ vs).all (\v.mem v kgvs) vs) kkk @ kg) in
--trace ("resolvedefs xs="@show_list show_int xs@"\nkg="@show_list prassert kg@"\nkgvs="@show_list show_int kgvs)
	resolvtyvars defs kk kg1
    end	

and resolvtyvars defs kk kg1 = -- ASSERT
	let idss = map (\(xs as (mkassert _ [v])._).(v, map (\(mkassert d _).d) xs)) (groupsort ltasv kg1) in
let rrr =
	case ynsplit (map (resolv1tyvar kk defs) idss) [] [] in
	    (vts, []) : ok [] [] vts
	||  (_, (s._)) : bad [s]
	end
in
if Test then trace (force ("resolvtyvars kg1="@show_list prassert kg1)@" rrr="@prTR rrr) rrr else
rrr

and ltasv (mkassert _ v1) (mkassert _ v2) = v1 < v2

-- Take a list of non-flat assertions and turn it into a flat one
and flatcollaps k =
    case oktestconc (map flatcon k) in
	(n as No _) : n
    ||  Yes k : Yes (subsume k)
    end

-- Take a single non-flat assertion and turn it into a flat assertion list
and flatcon (d, mktvar v) = Yes [mkassert d [v]]
||  flatcon ((d as mkid _ _ (idi_class (clsi _ _ _ _ insts _)) _), ot) =
    case while is_syn synexpand ot in
        (t as mktcons ti ts) : 
            case findit d t insts in
		[] : No ("[56] Not an instance "@nprttype (mktcons d [t]))
	    ||  [(_, mkidecl k _ [mktcons _ tvs] _)] : -- ASSERT
		    let su = matchs tvs ts in
                    let c = substcon su k in
		    case oktestconc (map flatcon c) in
			(n as No _) : n
		    ||  Yes k : Yes (subsume k)
		    end
            ||  xs : No ("[57] Ambiguous instances for "@prttype (mktcons d [t])@" "@show_list (pridecl o snd) xs)
            end
    ||  (t as mktap v ts) : --fail ("flatcon ap "@prttype (mktcons d [t])) -- XXX
            No ("[102] No instance "@prttype (mktcons d [t]))
    ||  (mktvar v) : Yes [mkassert d [v]]
    end
||  flatcon (i, v) = fail ("No match in flatcon "@dprid i)

and matcht x y = matchl [x] [y]
and matchs xs ys =
	case matchl xs ys in
	    None : fail ("matching error "@show_list prttype xs@" with "@show_list prttype ys)
	||  Some x : x
	end
and matchl xs ys = reduce cmatch (Some []) (map2 match xs ys)
and match (mktvar v) t = Some [(v, synexpand t)]
-- XXX mktap
||  match (t1 as mktcons i1 vs) (t2 as mktcons i2 ts) =
    if eqid i1 i2 then
	matchl vs ts
    else if id_issyn i1 then
	match (synexpand t1) t2
    else if id_issyn i2 then
	match t1 (synexpand t2)
    else
	None
||  match t1 t2 = None
and cmatch (Some xs) (Some ys) = cmatch1 xs ys
||  cmatch _ _ = None
and cmatch1 [] ys = Some ys
||  cmatch1 ((x as (v1,t1)).xs) ys =
	case findf ys (\(v2,_).v1=v2) Some None in
	    None : cmatch1 xs (x.ys)
	||  Some (_,t2) : if eqtype t1 t2 then cmatch1 xs ys else None
	end

-- reorder the current context according to another one.
-- also checks that the second is not more general than the first.
-- Is this check right??
and reorder (S as ok kall k1 ss) t =
    let k2 = cpart t in
    let k = map (tra ss) k2 in
    let kv = getTvars (TRtype S (tpart t)) in
    let (kin, kout) = partition (\(mkassert _ vs).all (\v.mem v kv) vs) k1 in
let rrr =
    if null (diffeq eqa kin k) then
	ok kall (k@kout) ss
    else
        let t' = TRtype S t in
        let [nt'; xt] = normtypes [t'; (mktcontype k1 (tpart t'))] in
	bad ["[58] Type restriction context not compatible with deduced: "@prttype t'@" vs. "@prttype xt]
in
if Test then trace ("reorder "@show_list prassert k1@" by "@show_list prassert k2@"("@show_list (show_list prassert) [k;kin;kout]@") is "@show_list prassert (k@kout)) rrr else
rrr
||  reorder S _ = S
and tra ss (mkassert ci vs) =
    mkassert ci (map (\v.
    case assocdef v ss (mktvar v) in
	mktvar vn : vn
    ||  _ : fail "Bad type var reorder"
    end) vs)
end
