module -- remsign
#include "../expr/id_t.t"
#include "../expr/id.t"
#include "../expr/types_t.t"
#include "../expr/ttype_t.t"
#include "../expr/ttype.t"
#include "../expr/tinfo.t"
#include "../expr/einfo_t.t"
#include "../expr/pprint.t"
#include "../expr/constr_t.t"
#include "../expr/constrfun.t"
#include "../rename/renameutil.t"
#include "../misc/misc.t"
#include "../misc/util.t"
#include "../misc/flags.t"
#include "../misc/sort.t"
#include "../type/conutil.t"		/* issuper, findit, himplies */
#include "../ExprE/classtrans.t"		/* xflatsuper */
#include "hexpr.t"			/* hiEval */
#include "lettrans.t"
#include "failcase.t"
#include "cutil.t"
#include "misc.t"
#include "../funnos.h"

-- Remove class and instance declarations.
-- Also do all remaing checks of validity.

export remclass;
rec remclass e = if Curry then rc e else e
and
    rc t =
	case t in
	   (mkap f a) :  mkap (rc f) (rc a)
	|| (mklam e1 e) : mklam e1 (rc e)
	|| (mkcase e cl) : mkcase (rc e) (map rcp cl)
	|| (mkletv b e) : mkletv (rcb b) (rc e)
	|| (mkident i) : t
	|| (mkmodule i fl il el b) : mkmodule i fl il el (rcb b)
	|| (mkconst _) : t
	|| (mkconstr c el) : mkconstr c (map (rc) el)
	|| (mkcondp p c) : mkcondp (rc p) (rc c)
	|| (mklazyp p) : mklazyp (rc p)
	|| (mkinfo t e) : mkinfo t (rc e)
	|| (mkerror _) : t
	|| (mkcfunction _ _) : t
	|| (mkfailmatch _) : t
	|| (mkwhere ges b) : mkwhere (map (\(g,e).rc g, rc e) ges) (rcb b)
	end
and
    rcb d =
	case d in
	   (mkbrec b) : mkbrec (rcb b)
	|| (mkband b1 b2) : mkband (rcb b1) (rcb b2)
	|| (mkblocal b1 b2) : mkblocal (rcb b1) (rcb b2)
	|| (mkbpat pl) : mkbpat (map rcp pl)
	|| (mkbmulti p e) : let (np,ne) = rcp (p,e) in mkbmulti np ne
	|| (mkbclass t b) : rclass t b
	|| (mkbinstance t b (Some i)) : evalchk t (rinst t b i)
        || (mkbview _ _ _ _) : mkbnull
        || _ : d
	end

    -- Pick out default methods and insert missing ones
and rclass cl b =
    case classchk cl in
	None :
        let (mkid _ _ (idi_class (clsi _ _ iits _ _ _)) _) = clsname cl in
	-- pick out default defs, and rename them
	let ies = concmap (pickbs (mkal iits)) (listify b) in
        -- make other default methods
	let ifs = map (\(d,m,t).mkbpat [(mkident d, farity t (mkinfo (restr [] t) (assocdefeq eqid d ies (mfail m))))]) iits in
	andify ifs
    ||  Some msg : mkberror msg
    end

and mkal iits = map (\(dmt as (_,m,_)).(idtostr m,dmt)) iits
and pickbs al (mkbpat [(mkident i, e)]) = [convid al i e]
||  pickbs _ _ = []
and convid al i e =
    let (d,m,t) = assocdef (idtostr i) al (fail ("convid "@idtostr i)) in
    (d, e)
and mfail m = tfail ("No default for "@oprid m)
-- Turn the instance declaration into a method vector.
-- t is the instance declaration, b are the bindings, vi is the method vector id (including method operator ids)
and rinst t b (vi as mkid un _ (idi_inst _ mis _) _) =
    case instchk t in
	None :
        let (mkidecl k (ci as mkid _ _ (idi_class (clsi _ _ iits sis _ nsup)) _) [it] _) = t in -- ASSERT
	let bs = map ipickbs (listify b) in
	let sm = length (filter (\(s, is).length is = 1) sis) in
	let nbs = map2 (makemetbind k bs) mis iits in
        -- unique numbers have been reserved in rename
        case mkmvec t un vi mis in
	    No msg : mkberror msg
        ||  Yes bs : andify (bs.nbs)
        end
    ||  Some msg : mkberror msg
    end
||  rinst _ _ vi = fail ("Bad rinst "@prid vi)


-- Make a binding for a method, use binding if present else make one to the default method
and makemetbind k bs mi (d,m,t) = 
    let (Ohastype tr _ _) = type_of_id mi in
    let (Ohastype (xxx as mktcontype (_.kd) td) _ _) = type_of_id d in
    let t' = xmkcontext k tr in
--trace(prttype tr@" "@prttype xxx@" "@prttype(mktcontype (k@kd) td)) (
    mkbpat [(mkident mi, mkinfo (restr [] t') (farity (mktcontype (k@kd) td) (assocdef (dropqual (idtostr m)) bs (mkident d))))]
--)
and ipickbs (mkbpat [(mkident i, e)]) = (/*idtostr*/snd (id_orignames i), e)
||  ipickbs b = --fail ("No match in ipickbs "@prdefg 0 b)
	fail "Sorry, no pattern bindings in instance declarations yet."
and xmkcontext [] t = t
||  xmkcontext ts (mktcontype ts' t) = mktcontype (ts@ts') t
||  xmkcontext ts t = mktcontype ts t

and farity t e =
	if DoForceArity then
	    mkinfo (forcearity (countarrows t)) e
	else
	    e

-- Build the method vector.  It's a function taking other vectors and then
-- an integer selector.  Don't typecheck this!
-- Unique numbers have been reserved by rename.
and mkmvec (id as mkidecl aas (ci as mkid _ _ (idi_class (clsi _ _ _ sups _ nsup)) _) _ _) u vi is = 
    let k = length aas in
    let tup = mkident (mknewid "dd" (u-k-2)) in
    let m = mkident (mknewid "mm" (u-k-1)) in
    let vecs = map mkident (map2 buildid aas (from (u-k))) in
    case mkmvec1 id vecs is in
       No x : No x
    || Yes es : 
        let f e = mklam m (mklam tup (mkinfo vectordef e)) in
	let g e =mkinfo vecreg2 case vecs in
		    []  : e
                 || [v] : mkletv (mkbpat [(v, tup)]) e
                 || _   : mkcase (mkinfo noeval tup) [(mkconstr (CTuple (length vecs)) vecs, e)]
                 end in
        let e = mkcase (mkinfo limitok (mkinfo noeval m)) (mapsnd g (addi es)) in
        let e' = f e in
        Yes (mkbpat [(mkident vi, mkinfo notchk e')])
    end
||  mkmvec (mkidecl _ ci _ _) _ _ _ = fail ("Bad mkmvec: "@prid ci)

and apl e l = revitlist (\a.\f.mkap f a) l e

and addi es = map2 (\n.\e.(mkconst (cint n), /*flatem*/ e)) (from 0) es
and mkmvec1 (id as mkidecl aas (mkid _ sss (idi_class (clsi _ _ _ sups _ nsup)) _) _ _) vecs is =
    let ses = map (bldsupdict id vecs) (head nsup sups) in
--trace ("mkmvec1 "@sss@" "@itos nsup@" "@show_list (prid o fst) sups@" "@show_list (\ (Yes(nsi,es)) . prid nsi @ ":" @ show_list ppr es) ses)
(
    let mpes = map (\i.mkinfo metcall (apl (mkident i) vecs)) is
    and spes = map (\(Yes (i, es)).apl (mkident i) es) ses in
    let sues = if FlatSuper then concmap supervec ses else [] in
--trace ("super "@show_list (\ (Yes (i,es)).prid i@show_list pprx es) ses)
--trace ("supervec "@pridecl id@" : "@show_list (show_ok (fail "NO") (show_list pprx)) xxx)
(
    findf ses isno (\ (No x).No x) (Yes (spes@mpes@sues))
)
)

and supervec (Yes ((vi as mkid un _ (idi_inst id mis _) _), es)) =
    case mkmvec1 id es mis in
       Yes es : es
    || No msg : fail ("supervec "@msg)
    end

-- Build the dictionaries for the superclasses, if possible
and bldsupdict (id as mkidecl aas ci [it as mktcons _ ts] _) vecs ((si as mkid _ _ (idi_class (clsi _ _ _ _ insts _)) _), _) = -- ASSERT
    let msg = prttype (idecl2type id) in
    case findit si it insts in
	[] : 
--trace ("ci="@prid ci@"\nit="@prttype it@"\nsi="@prid si@"\ninsts="@show_list (show_pair(prid,pridecl)) insts)
(
	    No ("[75] No instance for superclass "@oprid si@" in "@msg)
)
    ||  [(nsi, sid as mkidecl k _ [mktcons _ tvs] _)] : -- Check that the current context implies the superclass context -- ASSERT
	    let al = matchs tvs ts in
	    let xs = substcon al k in
	    if all (himplies aas) xs then
		-- Everything in the new context is implied
--trace ("bldsupdict "@prid nsi@" "@pridecl sid)
(
		Yes (nsi, map (erebuild (combine (vecs, aas))) xs)
)
	    else
		No ("[76] Instance context does not imply class context "@prttype (idecl2type sid)@", "@oprid si@" in "@msg)
    ||  xs : No ("[77] Too many superclass instances "@oprid si@" in "@msg@" "@show_list (pridecl o snd) xs)
    end
and pprx e = butlast (ppr e)


-- find closest superclass
-- take a list of (Id, Assert) pairs and a non-flat context and turn this into the appropriate dictionary expression
and erebuild iks dv = xrebuild mkint mkident apl iks dv

and rcp (p, e) = (p, rc e)

-- Check class declaration
and classchk (cd as mkcdecl aas a) =
    -- Check type variables
    if ~ allsame (getavars (a.aas)) then
	Some ("[78] Bad type variables in class declaration: "@prttype (cdecl2type cd))
    else
	None

and vischk i = id_visibility i = Vimported & ~ inprelude (id_orignames i)

-- Check instance declaration
and instchk (id as mkidecl aas ci [it as mktcons ti _] _) = -- ASSERT
    let sdcl = prttype (idecl2type id) in
    if vischk ci & vischk ti & ~ AllowRedef & ~LocalInst then
	Some ("[79] Type or class must be in this module: "@sdcl)
    -- Check type variables
    else if difference (getavars aas) (getTvars it) ~= [] then
	Some ("[80] Bad type variables in instance declaration: "@sdcl)
    else if id_issyn ti then
	Some ("[81] Type synonym in instance declaration: "@sdcl)
    else
	None
||  instchk (mkidecl _ ci [t] _) = fail ("No match in instchk: "@prid ci@" "@prttype t) -- ASSERT

and 
#if 0
    evalchk (mkidecl _ ci [mktcons tn _] _) r & (EvalClass & eqid ci hiEval) = -- ASSERT
    let (mkid _ _ (idi_type _ t _ ti _ _) _) = tn in
    let cs = get_cs_from_tinfo ti
    and iso = get_isotype ti in
    case [ t ;; (mkcons i xxx tbs sel) <- cs; (t,b,s) <- tbs; b | iso ] in
       [] : r
    || ss : 
       let c = cpart t in
       case filter (\ t . ~ himplies c (hiEval, t)) ss in
	  [] : r
       || ts : mkberror ("[109] Type definition '"@prttype t@"' should have Eval context '("@mix (map (\t.prttype (mktcons hiEval [t])) ts) "," @ ")'")
       end
    end
||  
#endif
    evalchk _ r = r
end
