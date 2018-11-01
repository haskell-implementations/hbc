module --caseutil
#include "../expr/id_t.t"
#include "../expr/id.t"
#include "../expr/constr_t.t"
#include "../expr/ttype_t.t"
#include "../expr/einfo_t.t"
#include "../expr/types_t.t"
#include "../expr/ttype.t"
#include "../expr/tinfo.t"
#include "../expr/constrfun.t"
#include "../expr/pprint.t"
#include "../rename/renenv.t"
#include "../misc/util.t"
#include "../misc/flags.t"
#include "misc.t"
#include "../expr/subst.t"
#include "cutil.t"
#include "failcase.t"
#include "exprs.t"
#include "hexpr.t"

export  mfail, gtCs, ispI, ispC, substas, dropcond, 
	overlap, rmas, rmstring, subfstp, typeerror, splitup, fixp,
	isconstp, cmpconst, countocc;

rec strtolist b = reduce echrcons enil b
and mfail e = (dollar, e)

and gtC (mkconstr n1 _) (mkconstr n2 _) = cno n1 > cno n2
and gtCs (x._, _) (y._, _) = gtC x y
and ispI (p._,_) = isI p
and ispC (p._,_) = isC p

and substas ei ((mkas i p).pl, e) = substas ei (p.pl, subst ei i e)
 || substas _  e = e

    -- Pattern overlap checking
    -- This is an n^2 algorithm!
and overlap [] = false
 || overlap (p.pl) = (Or (map (overl p) pl) | overlap pl
 	where rec overl (mkident _) _ = true
	       || overl (mkconstr c1 el1) (mkconstr c2 el2) =
			cno c1 = cno c2 & And (map2 overl el1 el2)
--	       || overl (mkinfo _ e1) e2 = overl e1 e2
--	       || overl e1 (mkinfo _ e2) = overl e1 e2
	       || overl _ _ = false )

and rmas (mkas i e) = rmas e
||  rmas (mkconstr c1 es) = mkconstr c1 (map rmas es)
||  rmas (mkcondp p c) = mkcondp (rmas p) c
||  rmas (mklazyp p) = mklazyp (rmas p)
--||  rmas (mkinfo t c) = mkinfo t (rmas c)
||  rmas (e as mkident _) = e
||  rmas (e as mkbrack _ _) = e    -- correct?? Annika 910702
--  nothing else can occur in a pattern

-- MKRESTR
-- remove strings and restr from patterns
and rmstring (mkconstr (c as Cconstr s _ _ _ _ _) _) & (isstring c) = strtolist s
||  rmstring (mkconstr c1 es) = mkconstr c1 (map rmstring es)
||  rmstring (mkas i e) = mkas i (rmstring e)
||  rmstring (mkcondp p c) = mkcondp (rmstring p) c
||  rmstring (mklazyp p) = mklazyp (rmstring p)
||  rmstring (mkinfo t p) = rmstring p
||  rmstring (e as mkident _) = e
||  rmstring (e as mkbrack _ _) = e        -- correct ?? Annika 910702
||  rmstring e = fail ("No match in rmstring "@ppr e)
-- nothing else can occur in a pattern

and subfstp ei = map (\(mkident pi.pl, e). pl, subst ei pi e)

and typeerror = not o allsameeq eqid o map (tname o tctype)
and dropcond (mkcondp p c) = p
||  dropcond p = p

and splitup = choplist (\l. let (cs, rs) = take ispI l in
			    let (ds, ns) = take ispC rs in
			    (cs@ds, ns))

-- Check if a = operation should be used instead of explicit code.
--- Explicit code saves time, but = test save space and compilation time,
and cntc (mkconstr _ es) = Sum (map cntc es) + 1
||  cntc (mkcondp p _) = cntc p
--||  cntc (mkinfo _ p) = cntc p
--||  cntc (mkas    _ p) = cntc p
||  cntc _             = -1000	-- Should suffice to make cntc negative
and isconstp p = ~Curry & cntc p > 4	-- arbitrary choice
and mkifc ce te fe = fmkcase ce [mfail (mkfailmatch 1);
				 (mkconstr hctrue [], te);
				 (mkconstr hcfalse [], fe)]
and cmpconst e1 e2 te fe = mkifc (mkap (mkap eeq e1) e2) te fe
and fixp (mkcondp p c, e) = ([p], mkifc c e (mkfailmatch 1))
||  fixp (p, e) = ([p], e)

-- count the number of i in e, but in a special way so occurences inside lamdba
-- counts higher (this is to avoid substituting inside lambda (bad for full lazyness)).
and countocc i e = (ee e
where rec
    ee (mkap e1 e2) = ee e1 + ee e2
||  ee (mklam i e) = 2 * ee e
||  ee (mkcase e pbl) = ee e + Sum (map ep pbl)
||  ee (mkletv b e) = eb b + ee e
||  ee (mkident ii) = if eqid i ii then 1 else 0
||  ee (mkerror emsg) = 0
||  ee (mkconstr _ el) = Sum (map ee el)
||  ee (mkfailmatch _) = 0
||  ee (mkinfo t e) = ee e
||  ee (mkcfunction _ _) = 0
||  ee e = fail ("No match in countocc: "@ppr e)

and eb (mkbpat pbl) = Sum (map ep pbl)
||  eb (mkband b1 b2) = eb b1 + eb b2
||  eb (mkbnull) = 0
||  eb (mkblocal b1 b2) = eb b1 + eb b2
||  eb (mkbrec b) = eb b
||  eb (mkbmulti p e) = ee p + ee e
||  eb (mkberror emsg) = 0

and ep (p, e) = ee e + epp p
and epp (mkcondp _ e) = ee e
||  epp _ = 0
)
end
