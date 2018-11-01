module -- remsign
#include "../misc/triple.t"
#include "../expr/id.t"
#include "../expr/id_t.t"
#include "../expr/types_t.t"
#include "../expr/ttype_t.t"
#include "../expr/ttype.t"
#include "../expr/einfo_t.t"
#include "../expr/pprint.t"
#include "../rename/renameutil.t"
#include "../misc/util.t"
#include "../misc/misc.t"
#include "../misc/flags.t"
#include "../type/conutil.t"		/* himplies */
#include "lettrans.t"
#include "hexpr.t"
#include "misc.t"

-- Remove type signatures and type synonymes
-- Also check context implication for all types.

export remsign, def_dflts;
rec remsign d e = rstop d e -- do this even for LML to remove mkbsyn
and def_dflts = [Tint; Tdfloat]
and rstop d (e as mkmodule i fl il el b) =
    case getdefs b in
	[] : (rs e, d)
    ||  [mkbdefault ts] : case partition okdef (map synexpandall ts) in
	                      (ts, []) : (rs e, ts)
                          ||  (_, ts) : (mkerror ("[38] Bad defaults: "@mix (map prttype ts) ", "), [])
			  end
    ||  _ : (mkerror "[39] More than one default", [])
    end

-- Check if t is an instance of c by looking for c in t's instances
and tisinstance c (mkid _ _ (idi_type _ _ _ _ is _) _) =
			    findf (map snd is) (\t.eqid c (iclsname t)) (\x.true) false

-- Is this enough?
and okdef (t as mktcons ti _) = tisinstance hiNum ti & getTvars t = []
||  okdef _ = false
and getdefs (mkbrec b) = getdefs b
||  getdefs (mkband b1 b2) = getdefs b1 @ getdefs b2
||  getdefs (d as mkbdefault ts) = [d]
||  getdefs (mkblocal _ b) = getdefs b
||  getdefs _ = []
and    
    rlex (mkunq e) = mkunq (rs e)
||  rlex x = x

and 
    rs t =
	case t in
	   (mkap f a) :  mkap (rs f) (rs a)
	|| (mklam e1 e) : mklam e1 (rs e)
	|| (mkcase e cl) : mkcase (rs e) (map rsp cl)
	|| (mkletv b e) : mkletv (rsb b) (rs e)
	|| (mkident i) : t
	|| (mkmodule i fl il el b) : mkmodule i fl il el (rsb b)
	|| (mkconst _) : t
	|| (mkcfunction _ _) : t
	|| (mkbrack g llex) : mkbrack g (map rlex llex)   -- conctypes
--	|| (mkas i p) : mkas i (rs p)
	|| (mkconstr c el) : mkconstr c (map rs el)
--	|| (mkcondp p c) : mkcondp (rs p) (rs c)
--	|| (mklazyp p) : mklazyp (rs p)
        || (mkinfo (f as restr _ t) e) :
		case typemsg (cpart t) (tpart t) in
		    [] : mkinfo f (rs e)
	        ||  ss : mkerror ("[40] Bad restriction: "@mix ss ", ")
		end
	|| (mkinfo f e) : mkinfo f (rs e)
	|| (mklistg e qs) : mklistg (rs e) (map rsq qs)
	|| (mkerror _) : t
	|| (mkfailmatch _) : t
	|| (mkwhere ges b) : mkwhere (map (\(g,e).rs g, rs e) ges) (rsb b)
	|| (mkrecord c ies cs) : mkrecord c (mapsnd rs ies) cs
        || _ : fail ("No match in rs (remsign) "@ppr t)
	end
and rsq (mkqfilter e) = mkqfilter (rs e)
||  rsq (mkqgen p e) = mkqgen (rspp p) (rs e)
||  rsq (mkqlet b) = mkqlet (rsb b)
and rspp (mkcondp p c) =
	mkcondp p (rspp c)
||  rspp p = p
and
    rsb d =
	case d in
	   (mkbrec b) : mkbrec (rsb b)
	|| (mkband b1 b2) : rems d
	|| (mkblocal b1 b2) : mkblocal (rsb b1) (rsb b2)
	|| (mkbpat pl) : mkbpat (map rsp pl)
	|| (mkbmulti p e) : let (np,ne) = rsp (p,e) in mkbmulti np ne
	|| (mkbsyn src dst) : 
#if 0
No contexts on synonyms any more
		case typemsg (cpart src) dst in
		    [] : mkbnull
		||  ss : mkberror ("[41] Bad synonym: "@mix ss ", ")
		end
#else
		mkbnull
#endif
        || (mkbinstance (t as mkidecl aas ci [it] _) b x) :  -- ASSERT
#if 0
No type implications are necessary for instances (?)
		case typemsg aas it in
		    [] : mkbinstance t (rsb b) x
		||  ss : mkberror ("[42] Bad instance: "@mix ss ", ")
		end
#else
		mkbinstance t (rsb b) x
#endif
	|| (mkbdefault _) : mkbnull
	|| (mkbtype t cs _ _) :
		case concmap (\(mkcons _ _ tbs _).
				  concmap (typemsg (cpart t) o fst3) tbs
		 	     )
			     cs in
		    [] : d
		||  ss : mkberror ("[43] Bad type def: "@mix ss ", ")
		end
	|| (mkbctype t prods) : d                  -- conctypes
		  -- extend it when Haskell 
        || (mkbclass cd b) : mkbclass cd (rsc b)
        || _ : d
	end
and
    rsc (mkbrec b) = mkbrec (rsc b)
||  rsc (mkband b1 b2) = mkband (rsc b1) (rsc b2)
||  rsc (mkbpat pl) = mkbpat (map rsp pl)
||  rsc b = b
and
    rsp (p, e) = (rspp p, rs e)
and
    rems d =
	let (ss, bs') = partition issign (listify d) in
        let sts = concmap flat ss in
	let dis = concmap getdi bs' in
        let sis = map fst sts in
	let badis = diffeq eqid sis dis in
	if anysameeq eqid (map fst sts) then
	    mkberror ("[44] Multiple signatures in "@mix (map oprid sis) ",")
	else if badis ~= [] & /* Temporary hack! */ diffeq eqid badis (concmap getalldi bs') ~= [] then
/*
trace (
prdefg 0 d@show_list prid dis@show_list prid sis
)
*/
(
	    mkberror ("[45] Id(s) "@mix (map oprid badis) ","@" not in scope for signature "@mix (map oprid sis) ",")
)
	else
	    andify (map (rsb o subsign sis sts) bs')
and issign (mkbsign _ _) = true
||  issign _ = false
and flat (mkbsign is t) = map (\i.(i, t)) is
and getdi (mkbpat [(mkident i, _)]) = [i]
||  getdi _ = []
-- getalldi is used as temporary hack for signatures
and getalldi (mkbmulti p _) = getids p
||  getalldi (mkbrec b) = concmap getalldi (listify b)
||  getalldi (mkblocal _ b) = concmap getalldi (listify b)
||  getalldi b = []
and subsign sis sts (mkbpat [(mkident i, e)]) & (member eqid i sis) = mkbpat [(mkident i, mkrestr e (assocdefeq eqid i sts (fail "subsign")))]
||  subsign sis sts (mkbrec b) = mkbrec (subsign sis sts b)
||  subsign sis sts (mkblocal b1 b2) = mkblocal b1 (subsign sis sts b2)
||  subsign sis sts (mkbmulti p e) = mkbmulti (subpat sis sts p) e
||  subsign _ _ b = b
and subpat sis sts (mkap f a) = mkap (subpat sis sts f) (subpat sis sts a)
||  subpat sis sts (ei as mkident i) & (member eqid i sis) = mkrestr ei (assocdefeq eqid i sts (fail "subpat"))
||  subpat sis sts (mkas i p) = mkas i (subpat sis sts p)
||  subpat sis sts (mkcondp p c) = mkcondp (subpat sis sts p) c
||  subpat sis sts (mklazyp p) = mklazyp (subpat sis sts p)
||  subpat _ _ p = p
and mkrestr e t = mkinfo (restr [] t) e

-- Check if all types are implied properly
and typemsg k ot =
    if ~Curry then
	[]
    else
	(tmsg ot
         where rec tmsg (mktcons (mkid _ _ (idi_syn _ t _ _) _) ts) = chkimp (cpart t) (tpart t) ts @ concmap tmsg ts
	       ||  tmsg (mktcons (mkid _ _ (idi_type _ t _ _ _ _) _) ts) = chkimp (cpart t) (tpart t) ts @ concmap tmsg ts
	       ||  tmsg _ = []
	 and       chkimp c (t as mktcons ti vs) ts =
		       let al = map2 (\(mktvar v).\t.(v, t)) vs ts in
		       let c' = map (\(mkassert ci [v]).(ci, assocdef v al (mktvar v))) c in -- ASSERT
		       if all (himplies k) c' then
			   []
		       else
			   ["[83] context not implied "@prttype ot@" (c'="@show_list (\(ci,t). prid ci@" ("@prttype t@")") c'@" by k="@prk k@")"]
	       ||  chkimp c t ts = [] -- must be an error
	 )
and prk c = show_list (prttype o as2ty) c
end
