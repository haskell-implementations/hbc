module
-- Generate a list (of ids) of possible derived instances
#include "../misc/triple.t"
#include "../expr/types_t.t"
#include "../expr/ttype_t.t"
#include "../expr/ttype.t"
#include "../expr/tinfo.t"
#include "../expr/id_t.t"
#include "../expr/id.t"
#include "../expr/pprint.t"
#include "../expr/booltree.t"
#include "../expr/einfo_t.t"
#include "../expr/einfo.t"
#include "../transform/hexpr.t"
#include "../transform/genderiv.t"
#include "../transform/lettrans.t"
#include "../misc/misc.t"
#include "../misc/flags.t"
#include "../type/conutil.t"		/* subsume */
#include "renenv.t"
#include "renameutil.t"
#include "../misc/util.t"
export solvederiv;
-- Solve the equation system that determines what can be derived.
-- This a hairy piece of code!  It's also very inefficient.  (Why include the Prelude every time?)
-- env is the environment created by this module, ienv is the imported (including Prelude) and
-- it should contain no automatic derivation.
--
-- env  is the environment from the current module,
-- ienv is the enviromnent from the imported stuff in the current module
-- penv is the enviromnent that already available (only used in the interactive system)
rec solvederiv auto ff env ienv penv u =
    let tdefs = filter istdef (rids Ktype env) in						-- type decls
    let itdefs = filter istdef (rids Ktype ienv) in
    let tinfo = map (info auto) (itdefs@tdefs) in
    if null tinfo then ([], rnil, mkbnull, u) else							-- fast special case
    let einsts = map ininfo (filter id_isinst (rids Kmeth env @ rids Kmeth ienv @ rids Kmeth penv)) in	-- explicit instance decls
--trace ("solvederiv "@show_list prid (rids Kmeth ienv))
--trace ("solvederiv "@show_list (\ (ci, ti, vs, k).pprid ci@":"@pprid ti) einsts@"\n"@show_list show1 tinfo)
(
    let (_, resx) = until eqls (itera einsts) ([], tinfo) in
    -- We finally know what can be derived, correlate this with given info and generate instance id's
    let errs = filter (errchk resx) tdefs in
    let res = map (\(i,_,_,_,kds).(i, kds)) resx in
    let (biss, u') = Umap (geni ff res) tdefs u in
    let (bs, is) = split (conc biss) in
    let (iis, u'') = Umap (genimpid res) itdefs u' in
    let b = andify bs
    and env = rlist Kmeth (is@conc iis) in
--    trace ("solve\n"@show_list show1 resx@"\n-----\n"@show_list prid errs)
--    (trace (concmap (\b.prdefg 0 b@"\n\n") bs))
(
    if Derived then
	(map oprid errs, env, b, u'')
    else
	([], rnil, mkbnull, u)
)
)

--and pras (mkassert c v) = oprid c @ " " @ prttype (mktvar v)
and pras = prttype o as2ty
and show1 = (\(x1,x2,x3,y,z).prttype (mktcons x1 (map mktvar x2))@ " = " @ show_list prttype y @ " " @ 
	                     show_list (show_pair((\x.show_list pras (x3@x)),prid)) (z)@"\n")


-- limit derivation according to obvious constraints, and reshape data
-- Returns (type name, type vars, context, used types, ([], possible classes))
and info auto (mkid _ _ (idi_type _ t _ ti _ od) _) =
    let cs = get_cs_from_tinfo ti in
    let ns = map (\(mkcons _ _ xs _).length xs) cs in
    case tpart t in
    mktcons ti vs :
    (ti, map getv vs, cpart t, gettsfromcs cs, map (\d.([],d)) 
                                          (diffeq eqid (full auto cs od)
					          ((if H1_3 & length vs ~= 1 then [hiFunctor] else [])@
						   (if all (\n.n = 0) ns then
						       []
					           else if length ns = 1 then
						       [hiEnum] @ if H1_3 then [/*hiBounded*/] else []
						   else
						       [hiEnum; hiIx] @ if H1_3 then [hiBounded] else []))))
    || _ : fail ("info: "@prttype t)
    end
||  info _ _ = fail "No match in info"

-- instance info: class name, type name, type vars, and context
and ininfo (mkid _ _ (idi_inst (mkidecl k ci [mktcons ti tvs] _) _ _) _) = (ci, ti, map (\(mktvar v).v) tvs, k) -- ASSERT

-- generate instance info from an type def. list
and iimpl (ti, vs, tk, _, kds) = map (\(k,d).(d, ti, vs, mkseteq eqass (tk@k))) kds
and eqass (mkassert i1 v1) (mkassert i2 v2) = eqid i1 i2 & v1 = v2
and ltass (mkassert i1 v1) (mkassert i2 v2) = ltid i1 i2

-- find type definitions that are interesting for deriving instances (ignore deriving ())
and istdef (mkid _ _ (idi_type _ _ _ ti _ x) _) = length (get_cs_from_tinfo ti) > 0 & ~ empty x
||  istdef _ = false
and empty (Some []) = true
||  empty _ = false
-- insert full derivation as first approx
and full auto cs (Some ds) = 
    mkseteq eqid ((if EvalClass&RelaxEval then [hiEval] else []) @
    if ~auto then 
	ds
    -- complete with superclasses
    else if member eqid hiEnum ds then
	ds @ [hiEq; hiOrd]
    else if H1_3 & member eqid hiBounded ds then
	ds @ [hiEq; hiOrd]
    else if member eqid hiIx ds then
	ds @ [hiEq; hiOrd]
    else if member eqid hiOrd ds then
	ds @ [hiEq]
    else
	ds)
||  full auto cs None = (if EvalClass&RelaxEval then [hiEval] else [])@if auto & (~LocalQuant | all (\(mkcons _ (_,vs,_) _ _).null vs) cs) then [hiEq; hiOrd; hiIx; hiEnum]@(if H1_3 then [hiBounded; hiShow; hiRead] else [hiBinary; hiText]) else []

and gettsfromcs cs = mkseteq eqtype (concmap (\(mkcons _ _ tbs _).map (synexpandall o fst3) tbs) cs)

-- compare two lists with instance info
and eqls (xs, ys) = And (map2 (\(_,_,_,_,xkds).\(y as (_,_,_,_,ykds)).And (map2 (\(xk,xd).\(yk,yd).eqid xd yd & seteq eqass xk yk) xkds ykds)) xs ys)

and itera eis (xxx, xs) = --trace ("iter1 old="@show_list show1 xxx@"\n      new="@show_list show1 xs) 
		(xs, iter (eis @ concmap iimpl xs) xs)			-- make one iteration
and iter ais xs = map (iter1 ais) xs
and iter1 ais (ti, vs, k, ts, kds) = 
    let iso = get_isotype (gettinfo ti)
    in  (ti, vs, k, ts, concmap (oned iso ais ts) kds)

and oned iso ais ts (_,d) = 
    case testconc (map (\t.ogetassert iso ais (d, t)) ts) in
	None : []
    ||  Some k : [(k,d)]
    end

-- get the assertions that makes a non-flat assertion true
and ogetassert iso its (d, (mktvar i)) = 
    if ~iso & EvalClass & eqid d hiEval then Some [] else Some [mkassert d [i]]
||  ogetassert iso its (d, (mktap v ts)) = 
    if ~iso & EvalClass & eqid d hiEval then Some [] else None
||  ogetassert iso its (d, (mktcons ti ts)) =
    if ~iso & EvalClass & eqid d hiEval then Some [] else 
    case getidt its d ti in
	None : None
    ||  Some (_, _, vs, k) :
             let al = combine (vs, ts) in
             let cn = map (ksubst al) k in
             testconc (map (ogetassert iso its) cn)
    end
||  ogetassert _ _ (_, mkterror _) = None

and ksubst al (mkassert d [v]) = (d, assocdef v al (mktvar v)) -- ASSERT
||  ksubst _  (mkaerror msg) = fail ("ksubst "@msg)

and getv (mktvar v) = v

and getidt xs sci sti = findf xs (\(ci, ti, _, _).eqid ci sci & eqid ti sti) Some None

-- Check for bad deriv
and errchk xs (i as mkid _ _ (idi_type _ _ _ _ _ (Some ds)) _) =
    let rds = findf xs (\(j,_,_,_,_).eqid j i) (\(_,_,_,_,kds).map snd kds) (fail ("errchk lookup"@prid i)) in
    ~all (\d.member eqid d rds) ds
||  errchk _ _ = false

-- generate instance declarations for a type def
and geni ff xs i u = 
    let kds = assocdefeq eqid i xs (fail ("geni lookup"@prid i)) in
    Umap (geni1 ff xs i) kds u

and geni1 ff xs (i as mkid _ _ (idi_type _ ty _ ti _ _) (Orignames _ _ (mi,_))) (ks, d) u =
    let iso = get_isotype ti in
    let (mktcons tid tvs) = tpart ty in
    let kk = /*if eqid d hiEval & not iso then [] else*/ cleanup (cpart ty @ ks) in
    let t = mkidecl kk d [mktcons tid tvs] (getminame mi) in -- ASSERT
    let (iid, u') = buildinstid ff u t t (cancmp d i) true (\_.\_.f_unk) in
    let (b, u'') = gender d ty (get_cs_from_tinfo ti) u' in
--trace ("geni1 "@pprid iid@" "@itos u'')
    ((mkbinstance t b (Some iid), iid), u'')
||  geni1 _ _ _ _ _ = fail "No match in geni1"

and cancmp c (i as mkid _ _ (idi_type _ _ _ ti _ _) _) = true	--!!! true is wrong!!!
#if 0
     all (isderivd i c) (get_cs_from_tinfo ti)
#endif

-- clean up an assertion
and cleanup ks = sort ltass (subsume ks)          --(mkseteq eqass ks)

and v n = btors [btvar n]
and v01 a = btands [v (a-2); v (a-1)]
and f01 a = finfo (-1) [] (v01 a, btff) (-1) None
and derfun s a = 	-- strictness info for derived functions
assocdef (idtostr s) [
("_==", f01);
("_/=", f01);
("_<=", f01);
("_>=", f01);
("_<",  f01);
("_>",  f01)
-- there are many more...
] (\_.f_unk) a

-- Generate an instance id for imported derived stuff
and genimpid xs i u =
    let kds = assocdefeq eqid i xs (fail ("genimpid lookup "@prid i@" "@show_list (prid o fst) xs)) in
    Umap (genimp1 xs i) kds u

and genimp1 xs (i as mkid _ _ (idi_type _ ty _ ti _ _) (Orignames vi _ (mi,_))) (ks, d) u =
    let (mktcons tid tvs) = tpart ty in
    let t = mkidecl (cleanup (cpart ty @ ks)) d [mktcons tid tvs] (getminame mi) in -- ASSERT
    let ff = (\i.Orignames vi Nofixity (mi, idtostr i)) in
    buildinstid ff u t t (cancmp d i) false derfun
||  genimp1 _ _ _ _ = fail " No match in genimp1"
end
